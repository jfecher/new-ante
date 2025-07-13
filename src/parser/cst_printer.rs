use std::{collections::HashMap, fmt::{Display, Formatter}};

use super::{cst::{Call, Cst, Definition, Expr, Index, Literal, MemberAccess, OwnershipMode, Path, Pattern, SequenceItem, TopLevelItem, TopLevelItemKind, Type, TypeDefinition, TypeDefinitionBody}, ids::{ExprId, PatternId}};

struct CstDisplayContext<'a> {
    indent_level: u32,
    exprs: &'a HashMap<ExprId, Expr>,
    patterns: &'a HashMap<PatternId, Pattern>,
}

impl Display for Cst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        CstDisplayContext { indent_level: 0 }.fmt_cst(self, f)
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut components = self.components.iter();

        if let Some(first) = components.next() {
            write!(f, "{first}")?;
        }

        while let Some(next) = components.next() {
            write!(f, ".{next}")?;
        }

        Ok(())
    }
}

impl CstDisplayContext {
    fn fmt_cst(&mut self, cst: &Cst, f: &mut Formatter) -> std::fmt::Result {
        for import in &cst.imports {
            writeln!(f, "import {}", import.path)?;
        }

        if !cst.imports.is_empty() {
            writeln!(f)?;
        }

        for item in &cst.top_level_items {
            self.fmt_top_level_item(item, f)?;
            writeln!(f)?;
        }

        Ok(())
    }

    fn fmt_top_level_item(&mut self, item: &TopLevelItem, f: &mut Formatter) -> std::fmt::Result {
        self.fmt_comments(&item.comments, f)?;
        match &item.kind {
            TopLevelItemKind::FunctionGroup(functions) => self.fmt_functions(functions, f),
            TopLevelItemKind::TypeDefinition(type_definition) => self.fmt_type_definition(type_definition, f),
        }
    }

    fn fmt_comments(&self, comments: &[String], f: &mut Formatter) -> std::fmt::Result {
        for comment in comments {
            writeln!(f, "{comment}")?;
            self.indent(f)?;
        }
        Ok(())
    }

    fn fmt_functions(&mut self, functions: &[Function], f: &mut Formatter) -> std::fmt::Result {
        for function in functions {
            self.fmt_function(function, f)?;
        }
        Ok(())
    }

    fn fmt_function(&mut self, function: &Function, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", function.path)?;

        for (name, typ) in &function.parameters {
            if name.item.is_empty() {
                write!(f, " ()")?;
            } else if let Some(typ) = typ {
                write!(f, " ({name}: ")?;
                self.fmt_type(typ, f)?;
                write!(f, ")")?;
            } else {
                write!(f, " {name}")?;
            }
        }

        if let Some(typ) = &function.return_type {
            write!(f, " : ")?;
            self.fmt_type(typ, f)?;
        }

        if let Some(expr) = &function.body {
            write!(f, " = ")?;
            self.fmt_expr(expr, f)?;
        }

        writeln!(f)
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
        write!(f, "type {} =", type_definition.name)?;

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

        writeln!(f)
    }

    fn fmt_type(&self, typ: &Type, f: &mut Formatter<'_>) -> std::fmt::Result {
        match typ {
            Type::Error => write!(f, "(error)"),
            Type::Named(path) => write!(f, "{path}"),
            Type::Unit => write!(f, "Unit"),
            Type::I32 => write!(f, "I32"),
            Type::U32 => write!(f, "U32"),
        }
    }

    fn fmt_expr(&mut self, expr: &Expr, f: &mut Formatter) -> std::fmt::Result {
        match expr {
            Expr::Error => write!(f, "(error)"),
            Expr::Literal(Literal::Integer(x, Some(kind))) => write!(f, "{x}_{kind}"),
            Expr::Literal(Literal::Integer(x, None)) => write!(f, "{x}"),
            Expr::Literal(Literal::String(s)) => write!(f, "\"{s}\""),
            Expr::Variable(path) => write!(f, "{path}"),
            Expr::Sequence(seq) => self.fmt_sequence(seq, f),
            Expr::Definition(definition) => self.fmt_definition(definition, f),
            Expr::Call(call) => self.fmt_call(call, f),
            Expr::MemberAccess(access) => self.fmt_member_access(access, f),
            Expr::Index(index) => self.fmt_index(index, f),
        }
    }

    fn fmt_sequence(&mut self, seq: &[SequenceItem], f: &mut Formatter) -> Result<(), std::fmt::Error> {
        self.indent_level += 1;
        for item in seq {
            self.newline(f)?;
            self.fmt_comments(&item.comments, f)?;
            self.fmt_expr(&item.expr, f)?;
        }
        self.indent_level -= 1;
        Ok(())
    }

    fn fmt_definition(&mut self, definition: &Definition, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if definition.mutable {
            write!(f, "mut ")?;
        }
        write!(f, "{}", definition.name)?;
        if let Some(typ) = &definition.typ {
            write!(f, ": ")?;
            self.fmt_type(typ, f)?;
        }

        write!(f, " = ")?;
        self.fmt_expr(&definition.rhs, f)?;
        Ok(())
    }

    fn fmt_call(&mut self, call: &Call, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.fmt_expr(&call.function, f)?;

        for arg in &call.arguments {
            if arg.is_atom() {
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

    fn fmt_member_access(&mut self, access: &MemberAccess, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if access.object.is_atom() {
            self.fmt_expr(&access.object, f)?;
        } else {
            write!(f, "(")?;
            self.fmt_expr(&access.object, f)?;
            write!(f, ")")?;
        }

        match access.ownership {
            OwnershipMode::Owned => write!(f, ".{}", access.member),
            OwnershipMode::Borrow => write!(f, ".&{}", access.member),
            OwnershipMode::BorrowMut => write!(f, ".!{}", access.member),
        }
    }

    fn fmt_index(&mut self, index: &Index, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if index.object.is_atom() {
            self.fmt_expr(&index.object, f)?;
        } else {
            write!(f, "(")?;
            self.fmt_expr(&index.object, f)?;
            write!(f, ")")?;
        }

        match index.ownership {
            OwnershipMode::Owned => write!(f, ".[")?,
            OwnershipMode::Borrow => write!(f, ".&[")?,
            OwnershipMode::BorrowMut => write!(f, ".![")?,
        }

        self.fmt_expr(&index.index, f)?;
        write!(f, "]")
    }
}
