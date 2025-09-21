use std::sync::Arc;

use crate::{diagnostics::{Diagnostic, Location}, incremental::GetType, name_resolution::{builtin::Builtin, Origin}, parser::{cst::{self, Definition, Expr, Literal, Pattern}, ids::{ExprId, PathId, PatternId}}, type_inference::{errors::TypeErrorKind, get_type::try_get_type, type_id::TypeId, types::{self, GeneralizedType, Type}, TypeChecker}};


impl<'local, 'inner> TypeChecker<'local, 'inner> {
    pub(super) fn check_definition(&mut self, definition: &Definition) -> GeneralizedType {
        let expected_generalized_type = try_get_type(definition, self.context, &self.resolve);
        let expected_type = expected_generalized_type.as_ref().map(|typ| typ.as_type(&mut self.types));

        let rhs_type = self.check_expr(definition.rhs, expected_type);
        self.check_pattern(definition.pattern, rhs_type);
        expected_generalized_type.unwrap_or_else(|| {
            self.generalize(rhs_type)
        })
    }

    fn check_expr(&mut self, expr: ExprId, expected: Option<TypeId>) -> TypeId {
        let typ = match &self.context.exprs[expr] {
            Expr::Literal(literal) => self.check_literal(literal),
            Expr::Variable(path) => self.path_type(*path, expected),
            Expr::Call(call) => self.check_call(call, expected),
            Expr::Lambda(lambda) => self.check_lambda(lambda, expected),
            Expr::Sequence(items) => {
                let mut typ = TypeId::UNIT;
                for (i, item) in items.iter().enumerate() {
                    let expected_type = (i == items.len() - 1).then_some(expected);
                    typ = self.check_expr(item.expr, expected_type.flatten());
                }
                typ
            },
            Expr::Definition(definition) => {
                self.check_definition(definition);
                TypeId::UNIT
            },
            Expr::MemberAccess(member_access) => self.check_member_access(member_access, expected),
            Expr::Index(index) => self.check_index(index, expected),
            Expr::If(if_) => self.check_if(if_, expected),
            Expr::Match(match_) => self.check_match(match_, expected),
            Expr::Reference(_) => todo!("type check references"),
            Expr::TypeAnnotation(type_annotation) => {
                let expected = self.types.convert_ast_type(&type_annotation.rhs);
                self.check_expr(type_annotation.lhs, Some(expected))
            },
            Expr::Handle(handle) => self.check_handle(handle, expected),
            Expr::Quoted(_) => todo!("type check Expr::Quoted"),
            Expr::Error => TypeId::ERROR,
        };
        self.expr_types.insert(expr, typ);
        typ
    }

    fn check_literal(&mut self, literal: &Literal) -> TypeId {
        match literal {
            Literal::Unit => TypeId::UNIT,
            Literal::Integer(_, Some(kind)) => TypeId::integer(*kind),
            Literal::Float(_, Some(kind)) => TypeId::float(*kind),
            Literal::Bool(_) => TypeId::BOOL,
            Literal::Integer(_, None) => TypeId::I32, // TODO: Polymorphic integers
            Literal::Float(_, None) => TypeId::F64, // TODO: Polymorphic floats
            Literal::String(_) => TypeId::STRING,
            Literal::Char(_) => TypeId::CHAR,
        }
    }

    fn check_pattern(&mut self, pattern: PatternId, actual_type: TypeId) {
        match &self.context.patterns[pattern] {
            Pattern::Error => (),
            Pattern::Variable(name) | Pattern::MethodName { item_name: name, .. } => {
                if let Some(existing) = self.name_types.get(name) {
                    self.unify(actual_type, *existing, TypeErrorKind::General, pattern);
                } else {
                    self.name_types.insert(*name, actual_type);
                }
            },
            Pattern::Literal(literal) => {
                let expected_type = self.check_literal(literal);
                self.unify(actual_type, expected_type, TypeErrorKind::General, pattern);
            },
            Pattern::Constructor(path, args) => {
                // TODO: How should we check `args`?
                let expected_type = self.path_type(*path, Some(actual_type));

                match self.types.get_type(expected_type) {
                    Type::Function(function) if !args.is_empty() => {
                        self.unify(function.return_type, actual_type, TypeErrorKind::General, pattern)
                    }
                    _ => self.unify(actual_type, expected_type, TypeErrorKind::General, pattern),
                }
            },
            Pattern::TypeAnnotation(inner_pattern, typ) => {
                let expected = self.types.convert_ast_type(typ);
                self.unify(actual_type, expected, TypeErrorKind::TypeAnnotationMismatch, pattern);
                self.check_pattern(*inner_pattern, expected);
            },
        }
    }
    
    fn path_type(&mut self, path: PathId, expected_type: Option<TypeId>) -> TypeId {
        match self.resolve.path_origins[&path] {
            Origin::TopLevelDefinition(top_level_id) => {
                let typ = GetType(top_level_id).get(self.compiler);
                self.instantiate(&typ)
            }
            Origin::Local(name) => self.name_types[&name],
            Origin::TypeResolution => todo!(),
            Origin::Builtin(builtin) => {
                let location = &self.context.path_locations[path];
                self.builtin_type(builtin, expected_type, location)
            }
        }
    }

    /// Returns the instantiated type of a builtin value
    ///
    /// Will error if passed a builtin type
    fn builtin_type(&mut self, builtin: Builtin, expected_type: Option<TypeId>, location: &Location) -> TypeId {
        match builtin {
            Builtin::Unit => TypeId::UNIT,
            Builtin::Int | Builtin::Char | Builtin::Float | Builtin::String | Builtin::PairType => {
                let typ = Arc::new(builtin.to_string());
                let location = location.clone();
                self.compiler.accumulate(Diagnostic::ValueExpected { location, typ });
                TypeId::ERROR
            },
            Builtin::PairConstructor => {
                if let Some(expected) = expected_type {
                    if let Type::Function(function) = self.types.get_type(expected) {
                        if function.parameters.len() == 2 {
                            if let Type::Application(constructor, args) = self.types.get_type(function.return_type) {
                                if *constructor == TypeId::PAIR && args.len() == 2 {
                                    return expected;
                                }
                            }
                        }
                    }
                }

                let a = self.next_type_variable();
                let b = self.next_type_variable();
                let pair = self.types.get_or_insert_type(Type::Application(TypeId::PAIR, vec![a, b]));
                let function = Type::Function(types::FunctionType {
                    parameters: vec![a, b],
                    return_type: pair,
                    effects: TypeId::UNIT,
                });
                self.types.get_or_insert_type(function)
            },
        }
    }

    fn check_call(&mut self, _call: &cst::Call, _expected: Option<TypeId>) -> TypeId {
        todo!()
    }

    fn check_lambda(&mut self, _lambda: &cst::Lambda, _expected: Option<TypeId>) -> TypeId {
        todo!()
    }

    fn check_member_access(&mut self, _member_access: &cst::MemberAccess, _expected: Option<TypeId>) -> TypeId {
        todo!()
    }

    fn check_index(&mut self, _index: &cst::Index, _expected: Option<TypeId>) -> TypeId {
        todo!()
    }

    fn check_if(&mut self, if_: &cst::If, expected: Option<TypeId>) -> TypeId {
        self.check_expr(if_.condition, Some(TypeId::BOOL));

        // If there's an else clause our expected return type should match the then/else clauses'
        // types. Otherwise, the then body may be any type.
        let expected = if if_.else_.is_some() { expected } else { None };
        let then_type = self.check_expr(if_.then, expected);

        match if_.else_ {
            Some(else_) => {
                let expected = expected.is_none().then_some(then_type);
                let else_type = self.check_expr(else_, expected);
                self.unify(else_type, then_type, TypeErrorKind::Else, else_);
                else_type
            }
            None => then_type,
        }
    }

    fn check_match(&mut self, match_: &cst::Match, expected: Option<TypeId>) -> TypeId {
        let expr_type = self.check_expr(match_.expression, None);
        let result_type = self.next_type_variable();
        let expected = expected.is_none().then_some(result_type);

        for (pattern, branch) in match_.cases.iter() {
            self.check_pattern(*pattern, expr_type);
            let branch_type = self.check_expr(*branch, expected);
            self.unify(branch_type, result_type, TypeErrorKind::MatchBranch, *branch);
        }

        result_type
    }

    fn check_handle(&mut self, _handle: &cst::Handle, _expected: Option<TypeId>) -> TypeId {
        todo!("check_handle")
    }
}
