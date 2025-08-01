use std::{collections::BTreeMap, fmt::{Display, Formatter}, sync::Arc};

use super::{cst::{BorrowMode, Call, Comptime, Cst, Declaration, Definition, EffectDefinition, EffectType, Expr, Extern, FunctionType, If, Import, Index, Lambda, Literal, Match, MemberAccess, OwnershipMode, Path, Pattern, Quoted, Reference, SequenceItem, SharedMode, TopLevelItem, TopLevelItemKind, TraitDefinition, TraitImpl, Type, TypeAnnotation, TypeDefinition, TypeDefinitionBody}, ids::{ExprId, PatternId, TopLevelId}, TopLevelContext};

pub struct CstDisplayContext<'a> {
    cst: &'a Cst,
    context: &'a BTreeMap<TopLevelId, Arc<TopLevelContext>>,
}

impl Cst {
    pub fn display<'a>(&'a self, context: &'a BTreeMap<TopLevelId, Arc<TopLevelContext>>) -> CstDisplayContext<'a> {
        CstDisplayContext { cst: self, context }
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut path = self.components.iter();
        write!(f, "{}", path.next().unwrap().0)?;
        for (item, _) in path {
            write!(f, ".{item}")?;
        }
        Ok(())
    }
}

/// This needs to be separate from `CstDisplayContext` since fmt requires `self` to be immutable
/// but we need to mutate indent_level.
struct CstDisplay<'a> {
    indent_level: u32,
    current_item: Option<TopLevelId>,
    context: &'a BTreeMap<TopLevelId, Arc<TopLevelContext>>,
}

impl Display for CstDisplayContext<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        CstDisplay { context: self.context, indent_level: 0, current_item: None }.fmt_cst(&self.cst, f)
    }
}

impl<'a> CstDisplay<'a> {
    fn fmt_cst(&mut self, cst: &Cst, f: &mut Formatter) -> std::fmt::Result {
        for import in &cst.imports {
            self.fmt_import(import, f)?;
            writeln!(f)?;
        }

        if !cst.imports.is_empty() {
            writeln!(f)?;
        }

        for item in &cst.top_level_items {
            self.current_item = Some(item.id);
            self.fmt_top_level_item(item, f)?;
            writeln!(f)?;
        }

        Ok(())
    }

    fn fmt_import(&mut self, import: &Import, f: &mut Formatter) -> std::fmt::Result {
        let path = import.module_path.to_string_lossy().replace("/", ".");
        write!(f, "import {path}.")?;

        for (i, (item, _location)) in import.items.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{item}")?;
        }
        Ok(())
    }

    fn context(&self) -> &'a TopLevelContext {
        &self.context[&self.current_item.unwrap()]
    }

    fn fmt_top_level_item(&mut self, item: &TopLevelItem, f: &mut Formatter) -> std::fmt::Result {
        self.fmt_comments(&item.comments, f)?;
        match &item.kind {
            TopLevelItemKind::TypeDefinition(type_definition) => self.fmt_type_definition(type_definition, f),
            TopLevelItemKind::Definition(definition) => self.fmt_definition(definition, f),
            TopLevelItemKind::TraitDefinition(trait_definition) => self.fmt_trait_definition(trait_definition, f),
            TopLevelItemKind::TraitImpl(trait_impl) => self.fmt_trait_impl(trait_impl, f),
            TopLevelItemKind::EffectDefinition(effect_definition) => self.fmt_effect_definition(effect_definition, f),
            TopLevelItemKind::Extern(extern_) => self.fmt_extern(extern_, f),
            TopLevelItemKind::Comptime(comptime) => self.fmt_comptime(comptime, f),
        }?;
        writeln!(f)
    }

    fn fmt_comments(&self, comments: &[String], f: &mut Formatter) -> std::fmt::Result {
        for comment in comments {
            writeln!(f, "{comment}")?;
            self.indent(f)?;
        }
        Ok(())
    }

    fn fmt_definition(&mut self, definition: &Definition, f: &mut Formatter) -> std::fmt::Result {
        if let Expr::Lambda(lambda) = &self.context().exprs[definition.rhs] {
            return self.fmt_function(definition, lambda, f);
        }

        if definition.mutable {
            write!(f, "mut ")?;
        }

        write!(f, "{}", definition.path)?;

        if let Some(typ) = &definition.typ {
            write!(f, ": ")?;
            self.fmt_type(typ, f)?;
        }

        write!(f, " =")?;
        if !matches!(self.context().exprs[definition.rhs], Expr::Sequence(_)) {
            write!(f, " ")?;
        }

        self.fmt_expr(definition.rhs, f)
    }

    fn fmt_function(&mut self, definition: &Definition, lambda: &Lambda, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", definition.path)?;
        self.fmt_lambda_inner(lambda, f)
    }

    /// Format each part of a lambda except the leading `fn`
    fn fmt_lambda_inner(&mut self, lambda: &Lambda, f: &mut Formatter) -> std::fmt::Result {
        for pattern in &lambda.parameters {
            write!(f, " ")?;
            self.fmt_pattern(*pattern, f)?;
        }

        if let Some(typ) = &lambda.return_type {
            write!(f, " : ")?;
            self.fmt_type(typ, f)?;
        }

        write!(f, " =")?;
        if !matches!(self.context().exprs[lambda.body], Expr::Sequence(_)) {
            write!(f, " ")?;
        }
        self.fmt_expr(lambda.body, f)
    }

    fn indent(&self, f: &mut Formatter) -> std::fmt::Result {
        for _ in 0 .. self.indent_level {
            write!(f, "    ")?;
        }
        Ok(())
    }

    fn newline(&self, f: &mut Formatter) -> std::fmt::Result {
        writeln!(f)?;
        self.indent(f)
    }

    fn fmt_type_definition(&mut self, type_definition: &TypeDefinition, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "type {} =", self.context().names[type_definition.name])?;

        match &type_definition.body {
            TypeDefinitionBody::Error => {
                write!(f, " (error)")?;
            },
            TypeDefinitionBody::Struct(fields) => {
                self.indent_level += 1;
                for (name, typ) in fields {
                    self.newline(f)?;
                    write!(f, "{name}: ")?;
                    self.fmt_type(typ, f)?;
                }
                self.indent_level -= 1;
            },
            TypeDefinitionBody::Enum(variants) => {
                self.indent_level += 1;
                for (name, params) in variants {
                    self.newline(f)?;
                    write!(f, "{name}")?;
                    for param in params {
                        write!(f, " ")?;
                        self.fmt_type(param, f)?;
                    }
                }
                self.indent_level -= 1;
            },
        }
        Ok(())
    }

    fn fmt_type(&self, typ: &Type, f: &mut Formatter<'_>) -> std::fmt::Result {
        match typ {
            Type::Error => write!(f, "(error)"),
            Type::Named(path) => write!(f, "{}", self.context().paths[*path]),
            Type::Variable(name) => write!(f, "{}", self.context().names[*name]),
            Type::Unit => write!(f, "Unit"),
            Type::Integer(kind) => write!(f, "{kind}"),
            Type::Function(function_type) => self.fmt_function_type(function_type, f),
            Type::TypeApplication(constructor, args) => self.fmt_type_application(constructor, args, f),
        }
    }

    fn fmt_type_application(&self, constructor: &Type, args: &[Type], f: &mut Formatter) -> std::fmt::Result {
        let requires_parens = |typ: &Type| {
            matches!(typ, Type::Function(_) | Type::TypeApplication(..))
        };

        if requires_parens(constructor) {
            write!(f, "(")?;
            self.fmt_type(constructor, f)?;
            write!(f, ")")?;
        } else {
            self.fmt_type(constructor, f)?;
        }

        for arg in args {
            if requires_parens(arg) {
                write!(f, " (")?;
                self.fmt_type(arg, f)?;
                write!(f, ")")?;
            } else {
                write!(f, " ")?;
                self.fmt_type(arg, f)?;
            }
        }

        Ok(())
    }

    fn fmt_function_type(&self, function_type: &FunctionType, f: &mut Formatter) -> std::fmt::Result {
        for parameter in &function_type.parameters {
            if matches!(parameter, Type::Function(_) | Type::TypeApplication(..)) {
                write!(f, "(")?;
                self.fmt_type(parameter, f)?;
                write!(f, ") -> ")?;
            } else {
                self.fmt_type(parameter, f)?;
                write!(f, " -> ")?;
            }
        }
        self.fmt_type(&function_type.return_type, f)?;

        if let Some(effects) = &function_type.effects {
            if effects.is_empty() {
                write!(f, " pure")?;
            } else {
                write!(f, " can ")?;
                for (i, effect) in effects.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    self.fmt_effect(effect, f)?;
                }
            }
        }
        Ok(())
    }

    fn fmt_effect(&self, effect: &EffectType, f: &mut Formatter) -> std::fmt::Result {
        match effect {
            EffectType::Known(path, args) => {
                write!(f, "{}", self.context().paths[*path])?;
                for arg in args {
                    write!(f, " ")?;
                    self.fmt_type(arg, f)?;
                }
                Ok(())
            },
            EffectType::Variable(name) => write!(f, "{}", self.context().names[*name]),
        }
    }

    fn fmt_expr(&mut self, expr: ExprId, f: &mut Formatter) -> std::fmt::Result {
        match &self.context().exprs[expr] {
            Expr::Error => write!(f, "(error)"),
            Expr::Literal(literal) => self.fmt_literal(literal, f),
            Expr::Variable(path) => write!(f, "{}", self.context().paths[*path]),
            Expr::Sequence(seq) => self.fmt_sequence(seq, f),
            Expr::Definition(definition) => self.fmt_definition(definition, f),
            Expr::Call(call) => self.fmt_call(call, f),
            Expr::MemberAccess(access) => self.fmt_member_access(access, f),
            Expr::Index(index) => self.fmt_index(index, f),
            Expr::Lambda(lambda) => self.fmt_lambda(lambda, f),
            Expr::If(if_) => self.fmt_if(if_, f),
            Expr::Match(match_) => self.fmt_match(match_, f),
            Expr::Reference(reference) => self.fmt_reference(reference, f),
            Expr::TypeAnnotation(type_annotation) => self.fmt_type_annotation(type_annotation, f),
            Expr::Quoted(quoted) => self.fmt_quoted(quoted, f),
        }
    }

    fn fmt_literal(&mut self, literal: &Literal, f: &mut Formatter) -> std::fmt::Result {
        match literal {
            Literal::Integer(x, Some(kind)) => write!(f, "{x}_{kind}"),
            Literal::Integer(x, None) => write!(f, "{x}"),
            Literal::String(s) => write!(f, "\"{s}\""),
            Literal::Unit => write!(f, "()"),
        }
    }

    fn fmt_sequence(&mut self, seq: &[SequenceItem], f: &mut Formatter) -> std::fmt::Result {
        self.indent_level += 1;
        for item in seq {
            self.newline(f)?;
            self.fmt_comments(&item.comments, f)?;
            self.fmt_expr(item.expr, f)?;
        }
        self.indent_level -= 1;
        Ok(())
    }

    fn fmt_call(&mut self, call: &Call, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt_expr(call.function, f)?;

        for arg in call.arguments.iter().copied() {
            if self.context().exprs[arg].is_atom() {
                write!(f, " ")?;
                self.fmt_expr(arg, f)?;
            } else {
                write!(f, " (")?;
                self.fmt_expr(arg, f)?;
                write!(f, ")")?;
            }
        }

        Ok(())
    }

    fn fmt_member_access(&mut self, access: &MemberAccess, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.context().exprs[access.object].is_atom() {
            self.fmt_expr(access.object, f)?;
        } else {
            write!(f, "(")?;
            self.fmt_expr(access.object, f)?;
            write!(f, ")")?;
        }

        match access.ownership {
            OwnershipMode::Owned => write!(f, ".{}", access.member),
            OwnershipMode::Borrow => write!(f, ".&{}", access.member),
            OwnershipMode::BorrowMut => write!(f, ".!{}", access.member),
        }
    }

    fn fmt_index(&mut self, index: &Index, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.context().exprs[index.object].is_atom() {
            self.fmt_expr(index.object, f)?;
        } else {
            write!(f, "(")?;
            self.fmt_expr(index.object, f)?;
            write!(f, ")")?;
        }

        match index.ownership {
            OwnershipMode::Owned => write!(f, ".[")?,
            OwnershipMode::Borrow => write!(f, ".&[")?,
            OwnershipMode::BorrowMut => write!(f, ".![")?,
        }

        self.fmt_expr(index.index, f)?;
        write!(f, "]")
    }

    fn fmt_declaration(&self, declaration: &Declaration, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}: ", self.context().names[declaration.name])?;
        self.fmt_type(&declaration.typ, f)
    }

    fn fmt_trait_definition(&mut self, trait_definition: &TraitDefinition, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "trait {}", self.context().names[trait_definition.name])?;

        for generic in &trait_definition.generics {
            write!(f, " {}", self.context().names[*generic])?;
        }

        if !trait_definition.functional_dependencies.is_empty() {
            write!(f, " ->")?;
            for generic in &trait_definition.functional_dependencies {
                write!(f, " {}", self.context().names[*generic])?;
            }
        }

        write!(f, " with")?;
        self.indent_level += 1;
        for declaration in &trait_definition.body {
            self.newline(f)?;
            self.fmt_declaration(declaration, f)?;
        }
        self.indent_level -= 1;
        Ok(())
    }

    fn fmt_trait_impl(&mut self, trait_impl: &TraitImpl, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "impl {}", self.context().paths[trait_impl.trait_path])?;

        for argument in &trait_impl.arguments {
            write!(f, " ")?;
            self.fmt_type(argument, f)?;
        }

        write!(f, " with")?;
        self.indent_level += 1;
        for definition in &trait_impl.body {
            self.newline(f)?;
            self.fmt_definition(definition, f)?;
        }
        self.indent_level -= 1;
        Ok(())
    }

    fn fmt_effect_definition(&mut self, effect_definition: &EffectDefinition, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "effect {}", self.context().names[effect_definition.name])?;

        for generic in &effect_definition.generics {
            write!(f, " {}", self.context().names[*generic])?;
        }

        write!(f, " with")?;
        self.indent_level += 1;
        for declaration in &effect_definition.body {
            self.newline(f)?;
            self.fmt_declaration(declaration, f)?;
        }
        self.indent_level -= 1;
        Ok(())
    }

    fn fmt_extern(&mut self, extern_: &Extern, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "extern ")?;
        self.fmt_declaration(&extern_.declaration, f)
    }

    fn fmt_lambda(&mut self, lambda: &Lambda, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn")?;
        self.fmt_lambda_inner(lambda, f)
    }

    fn fmt_if(&mut self, if_: &If, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ")?;
        self.fmt_expr(if_.condition, f)?;
        write!(f, " then ")?;
        self.fmt_expr(if_.then, f)?;

        if let Some(else_) = if_.else_ {
            write!(f, " else ")?;
            self.fmt_expr(else_, f)?;
        }
        Ok(())
    }

    fn fmt_match(&mut self, match_: &Match, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "match ")?;
        self.fmt_expr(match_.expression, f)?;

        for (pattern, branch) in &match_.cases {
            self.newline(f)?;
            write!(f, "| ")?;
            self.fmt_pattern(*pattern, f)?;
            write!(f, " -> ")?;
            self.fmt_expr(*branch, f)?;
        }

        Ok(())
    }

    fn fmt_pattern(&mut self, pattern: PatternId, f: &mut Formatter) -> std::fmt::Result {
        match &self.context().patterns[pattern] {
            Pattern::Variable(name) => write!(f, "{}", self.context().names[*name]),
            Pattern::Literal(literal) => self.fmt_literal(literal, f),
            Pattern::Constructor(path, args) => {
                self.fmt_pattern(*path, f)?;
                for arg in args {
                    if !matches!(&self.context().patterns[*arg], Pattern::Constructor(..)) {
                        write!(f, " ")?;
                        self.fmt_pattern(*arg, f)?;
                    } else {
                        write!(f, " (")?;
                        self.fmt_pattern(*arg, f)?;
                        write!(f, ")")?;
                    }
                }
                Ok(())
            },
            Pattern::Error => write!(f, "(error)"),
            Pattern::TypeAnnotation(pattern, typ) => {
                write!(f, "(")?;
                self.fmt_pattern(*pattern, f)?;
                write!(f, ": ")?;
                self.fmt_type(typ, f)?;
                write!(f, ")")
            },
        }
    }

    fn fmt_reference(&mut self, reference: &Reference, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", reference.mode)?;
        if !reference.mode.is_shared() {
            write!(f, " ")?;
        }
        self.fmt_expr(reference.rhs, f)
    }

    fn fmt_type_annotation(&mut self, type_annotation: &TypeAnnotation, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt_expr(type_annotation.lhs, f)?;
        write!(f, ": ")?;
        self.fmt_type(&type_annotation.rhs, f)
    }

    fn fmt_comptime(&mut self, comptime: &Comptime, f: &mut Formatter<'_>) -> std::fmt::Result {
        match comptime {
            Comptime::Expr(expr_id) => {
                write!(f, "#")?;
                self.fmt_expr(*expr_id, f)
            }
            Comptime::Derive(paths) => {
                write!(f, "derive")?;
                for path in paths {
                    write!(f, " {}", self.context().paths[*path])?;
                }
                Ok(())
            },
            Comptime::Definition(definition) => {
                write!(f, "#")?;
                self.fmt_definition(definition, f)
            },
        }
    }

    fn fmt_quoted(&self, quoted: &Quoted, f: &mut Formatter<'_>) -> std::fmt::Result {
        assert!(!quoted.tokens.is_empty());
        write!(f, "'{}", quoted.tokens.first().unwrap())?;

        for token in quoted.tokens.iter().skip(1) {
            write!(f, " {token}")?;
        }
        Ok(())
    }
}

impl Display for BorrowMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BorrowMode::Immutable(shared_mode) => write!(f, "&{shared_mode}"),
            BorrowMode::Mutable(shared_mode) => write!(f, "!{shared_mode}"),
        }
    }
}

impl Display for SharedMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SharedMode::Shared => Ok(()),
            SharedMode::Owned => write!(f, "own"),
        }
    }
}
