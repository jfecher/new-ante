use std::{collections::HashMap, sync::Arc};

use cst::{BorrowMode, Index, Lambda, MemberAccess, OwnershipMode, Pattern, SharedMode};
use ids::{ExprId, PatternId, TopLevelId};

use crate::{errors::{Diagnostic, ErrorDefault, Location, Span}, lexer::{token::Token, Lexer}, vecmap::VecMap};

use self::cst::{Cst, Import, Path, TopLevelItem, TopLevelItemKind, TypeDefinition, Type, Expr, TypeDefinitionBody, Literal, SequenceItem, Definition, Call};

pub mod cst;
pub mod cst_printer;
pub mod ids;

pub struct ParseResult {
    pub cst: Cst,
    pub diagnostics: Vec<Diagnostic>,
    pub top_level_data: HashMap<TopLevelId, TopLevelContext>,
}

/// Metadata associated with a top level statement
#[derive(Default)]
pub struct TopLevelContext {
    pub exprs: VecMap<ExprId, Expr>,
    pub patterns: VecMap<PatternId, Pattern>,
    pub expr_locations: VecMap<ExprId, Location>,
    pub pattern_locations: VecMap<PatternId, Location>,
}

type Result<T> = std::result::Result<T, Diagnostic>;

struct Parser<'tokens> {
    file_path: Arc<String>,
    tokens: &'tokens [(Token, Span)],
    diagnostics: Vec<Diagnostic>,
    top_level_data: HashMap<TopLevelId, TopLevelContext>,

    /// Keep track of any name collisions in the top level items
    top_level_item_count: HashMap<String, /*collision count:*/u32>,

    current_context: TopLevelContext,

    token_index: usize,
}

pub fn parse_file(file_path: Arc<String>, file_contents: &str) -> ParseResult {
    let tokens = Lexer::new(&file_contents).collect::<Vec<_>>();
    Parser::new(file_path, &tokens).parse()
}

impl<'tokens> Parser<'tokens> {
    fn new(file_path: Arc<String>, tokens: &'tokens [(Token, Span)]) -> Self {
        Self {
            file_path,
            tokens,
            diagnostics: Vec::new(),
            token_index: 0,
            top_level_data: Default::default(),
            top_level_item_count: Default::default(),
            current_context: Default::default(),
        }
    }

    fn parse(mut self) -> ParseResult {
        let imports = self.parse_imports();
        let top_level_items = self.parse_top_level_items();
        let cst = Cst { imports, top_level_items };
        ParseResult {
            cst,
            diagnostics: self.diagnostics,
            top_level_data: self.top_level_data,
        }
    }

    fn current_token(&self) -> &'tokens Token {
        &self.tokens[self.token_index].0
    }

    fn current_token_span(&self) -> Span {
        self.tokens[self.token_index].1
    }

    fn current_token_location(&self) -> Location {
        self.current_token_span().in_file(self.file_path.clone())
    }

    /// Returns the previous token, if it exists.
    /// Returns the current token otherwise.
    fn previous_token(&self) -> &'tokens Token {
        &self.tokens[self.token_index.saturating_sub(1)].0
    }

    /// Returns the previous token's span, if it exists.
    /// Returns the current token's span otherwise.
    fn previous_token_span(&self) -> Span {
        self.tokens[self.token_index.saturating_sub(1)].1
    }

    /// Returns the next token.
    /// Panics if the current token is EOF
    fn next_token(&self) -> &'tokens Token {
        &self.tokens[self.token_index + 1].0
    }

    fn current_token_and_span(&self) -> &'tokens (Token, Span) {
        &self.tokens[self.token_index]
    }

    fn advance(&mut self) {
        self.token_index += 1;
    }

    /// Advance the input if the current token matches the given token.
    /// Returns true if we advanced the input.
    fn accept(&mut self, token: Token) -> bool {
        if *self.current_token() == token {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Advance the input if the current token matches the given token, or error otherwise.
    /// Returns true if we advanced the input
    fn expect(&mut self, token: Token, message: &'static str) -> bool {
        match self.try_expect(token, message) {
            Ok(_) => true,
            Err(error) => {
                self.diagnostics.push(error);
                false
            }
        }
    }

    fn try_expect(&mut self, token: Token, message: &'static str) -> Result<()> {
        if self.accept(token) {
            Ok(())
        } else {
            let actual = self.current_token().clone();
            let location = self.current_token_span().in_file(self.file_path.clone());
            Err(Diagnostic::ParserExpected { message, actual, location })
        }
    }

    /// Reserve a space for an expression.
    /// This can be more cache efficient to reserve the spaces for parent expressions
    /// before their children.
    fn reserve_expr(&mut self) -> ExprId {
        let id = self.current_context.exprs.push(Expr::Error);
        let id2 = self.current_context.expr_locations.push(self.current_token_location());
        assert_eq!(id, id2);
        id
    }

    fn insert_expr(&mut self, id: ExprId, expr: Expr, location: Location) {
        self.current_context.exprs[id] = expr;
        self.current_context.expr_locations[id] = location;
    }

    fn push_expr(&mut self, expr: Expr, location: Location) -> ExprId {
        let id = self.current_context.exprs.push(expr);
        let id2 = self.current_context.expr_locations.push(location);
        assert_eq!(id, id2);
        id
    }

    /// Create a new TopLevelId from the name of a given top level item.
    /// In the case of definitions, this name will be only the last element in their path.
    fn new_top_level_id(&mut self, name: String) -> TopLevelId {
        // Check for previous name collisions to disambiguate the resulting hash
        let collision = *self.top_level_item_count.entry(name.clone())
            .and_modify(|count| *count += 1)
            .or_default();

        let id = TopLevelId::new_named(self.file_path.clone(), &name, collision);
        self.top_level_data.insert(id.clone(), std::mem::take(&mut self.current_context));
        id
    }

    /// Return the location of an ExprId within the current context
    fn expr_location(&self, expr: ExprId) -> Location {
        self.current_context.expr_locations[expr].clone()
    }

    /// Skip all tokens up to the next newline (or unindent) token.
    /// If an Indent token is encountered we'll try to match indents an unindents
    /// so that any newlines in between are skipped.
    fn recover_to_next_newline(&mut self) {
        let mut indents = 0;

        loop {
            match self.current_token() {
                Token::Newline => {
                    if indents == 0 {
                        break;
                    }
                }
                Token::Indent => indents += 1,
                Token::Unindent => {
                    // Since we could recover from anywhere in the program, its possible
                    // we recover from, e.g. the middle of a function body and hit an
                    // unindent before we hit a newline.
                    if indents == 0 {
                        break;
                    } else {
                        indents -= 1;
                    }
                }
                Token::EndOfInput => break,
                _ => (),
            }
            self.advance();
        }

        while *self.current_token() != Token::Newline {
            self.advance();
        }
    }

    /// Try to recover to the target token (not consuming it), stopping
    /// early if any of the `too_far` tokens (or EOF) are found.
    /// Returns `true` if we successfully recovered, or `false` if any
    /// of the `too_far` tokens were encountered first.
    fn recover_to(&mut self, target: Token, too_far: &[Token]) -> bool {
        loop {
            let token = self.current_token();

            if *token == target {
                break true;
            } else if *token == Token::EndOfInput || too_far.contains(token) {
                break false;
            } else {
                self.advance();
            }
        }
    }

    /// Try to parse an item using the given parser. On failure,
    /// report the error and recover by skipping all tokens up to
    /// next newline or unindent token.
    ///
    /// Note that this will also attempt to match indents to unindents.
    /// So any newlines within indented blocks will be skipped until an
    /// unbalanced unindent or newline on the same indentation level is found.
    fn try_parse_or_recover_to_newline<T>(&mut self, parser: impl FnOnce(&mut Self) -> Result<T>) -> Option<T> {
        match parser(self) {
            Ok(item) => Some(item),
            Err(error) => {
                self.diagnostics.push(error);
                self.recover_to_next_newline();
                None
            }
        }
    }

    /// Try to parse an item, pushing the parse error on failure and returning the ErrorDefault
    /// value for the given type. This makes no attempt to recover on parse failure - the input
    /// will be left at the same position.
    fn try_parse<T: ErrorDefault>(&mut self, parser: impl FnOnce(&mut Self) -> Result<T>) -> T {
        match parser(self) {
            Ok(item) => item,
            Err(error) => {
                self.diagnostics.push(error);
                T::error_default()
            }
        }
    }

    /// Run the given parse function and return its result on success.
    ///
    /// On error, try to recover to the given token, stopping short if any of
    /// the `too_far` tokens (or EOF) are found first. On a successful recovery,
    /// return the given default error value. Otherwise return the original error.
    fn parse_with_recovery<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T>, recover_to: Token, too_far: &[Token]) -> Result<T>
        where T: ErrorDefault
    {
        match f(self) {
            Ok(typ) => Ok(typ),
            Err(error) => {
                if self.recover_to(recover_to, too_far) {
                    self.diagnostics.push(error);
                    Ok(T::error_default())
                } else {
                    Err(error)
                }
            }
        }
    }

    fn parse_imports(&mut self) -> Vec<Import> {
        let mut imports = Vec::new();

        while self.accept(Token::Import) {
            if let Some(path) = self.try_parse_or_recover_to_newline(Self::parse_type_or_value_path) {
                imports.push(Import { path });
            }
            self.expect(Token::Newline, "newline after import");
        }

        imports
    }

    // value_path: (typename '.')* ident
    fn parse_value_path(&mut self) -> Result<Path> {
        let mut components = Vec::new();

        while let Ok(typename) = self.parse_type_name() {
            components.push(typename);
            self.try_expect(Token::MemberAccess, "`.` after module name")?;
        }

        components.push(self.parse_ident()?);
        Ok(Path { components })
    }

    // type_path: (typename '.')* typename
    fn parse_type_path(&mut self) -> Result<Path> {
        let mut components = vec![self.parse_type_name()?];

        while self.accept(Token::MemberAccess) {
            components.push(self.parse_type_name()?);
        }

        Ok(Path { components })
    }

    // type_or_value_path: (typename '.')* (ident | typename)
    fn parse_type_or_value_path(&mut self) -> Result<Path> {
        let mut components = Vec::new();

        while let Ok(typename) = self.parse_type_name() {
            components.push(typename);

            if !self.accept(Token::MemberAccess) {
                return Ok(Path { components });
            }
        }

        // If we made it here we had a trailing `.` but the token after it was not a typename,
        // so it must be a variable name.
        components.push(self.parse_ident()?);
        Ok(Path { components })
    }

    fn parse_top_level_items(&mut self) -> Vec<TopLevelItem> {
        let mut items = Vec::new();

        while *self.current_token() != Token::EndOfInput {
            if let Some(item) = self.try_parse_or_recover_to_newline(Self::parse_top_level_item) {
                items.push(item);
            }
            self.expect(Token::Newline, "a newline after the top level item");
        }

        items
    }

    fn parse_top_level_item(&mut self) -> Result<TopLevelItem> {
        let comments = self.parse_comments();
        let id: TopLevelId;

        let kind = match self.current_token() {
            Token::Identifier(_) | Token::TypeName(_) => {
                let definition = self.parse_definition()?;
                // parse_definition can eat the trailing newline
                if *self.previous_token() == Token::Newline {
                    self.token_index -= 1;
                }

                id = self.new_top_level_id(definition.path.last().clone());
                TopLevelItemKind::Definition(definition)
            }
            Token::Type => {
                let definition = self.parse_type_definition()?;
                id = self.new_top_level_id(definition.name.clone());
                TopLevelItemKind::TypeDefinition(definition)
            }
            other => {
                let message = "a top-level item";
                let location = self.current_token_location();
                return Err(Diagnostic::ParserExpected { message, actual: other.clone(), location });
            }
        };

        Ok(TopLevelItem { id, comments, kind })
    }

    fn parse_comments(&mut self) -> Vec<String> {
        let mut comments = Vec::new();

        while let Token::LineComment(comment) = self.current_token() {
            comments.push(comment.clone());
            self.advance();
            self.expect(Token::Newline, "newline after comment");
        }

        comments
    }

    fn parse_definition(&mut self) -> Result<Definition> {
        let mutable = self.accept(Token::Mut);

        let start_location = self.current_token_location();
        let path = self.parse_value_path()?;
        let parameters = self.parse_function_parameters();

        // If this is a lambda, reserve the lambda's expr id ahead of time so it is allocated
        // before the body for better cache performance
        let lambda = (parameters.len() != 0).then(|| self.reserve_expr());

        let mut typ = None;
        if self.accept(Token::Colon) {
            typ = self.parse_with_recovery(Self::parse_type, Token::Equal, &[Token::Newline, Token::Indent]).ok();
        }
        self.expect(Token::Equal, "`=` to begin the function body");

        let rhs = self.try_parse_or_recover_to_newline(|this| this.parse_expression()).unwrap_or_else(|| {
            self.push_expr(Expr::Error, self.current_token_location())
        });

        if let Some(id) = lambda {
            let lambda = Expr::Lambda(Lambda {
                parameters,
                return_type: typ,
                body: rhs,
            });
            self.insert_expr(id, lambda, start_location);
            Ok(Definition { mutable, path, typ: None, rhs: id })
        } else {
            Ok(Definition { mutable, path, typ, rhs })
        }
    }

    fn parse_type_definition(&mut self) -> Result<TypeDefinition> {
        self.expect(Token::Type, "`type`");
        let name = self.parse_type_name()?;
        let generics = self.many0(|this| this.parse_ident());
        self.expect(Token::Equal, "`=` to begin the type definition");
        let body = self.parse_type_body()?;
        Ok(TypeDefinition { name, generics, body })
    }

    fn parse_type_body(&mut self) -> Result<TypeDefinitionBody> {
        match self.current_token() {
            Token::Indent => Ok(self.parse_indented(Self::parse_indented_type_body)),
            _ => self.parse_non_indented_type_body(),
        }
    }

    fn parse_indented_type_body(&mut self) -> Result<TypeDefinitionBody> {
        match self.current_token() {
            // struct
            Token::Identifier(_) => {
                let fields = self.delimited(|this| {
                    let field_name = this.parse_ident()?;
                    this.expect(Token::Colon, "a colon separating the field name from its type");
                    let field_type = this.parse_type()?;
                    this.accept(Token::Comma);
                    Ok((field_name, field_type))
                }, Token::Newline, false);
                Ok(TypeDefinitionBody::Struct(fields))
            }
            // enum
            _ => {
                let variants = self.delimited(|this| {
                    this.expect(Token::Pipe, "`|`");
                    let variant_name = this.parse_type_name()?;
                    let parameters = this.many0(Self::parse_type); // TODO: arg type
                    Ok((variant_name, parameters))
                }, Token::Newline, false);
                Ok(TypeDefinitionBody::Enum(variants))
            }
        }
    }

    fn parse_non_indented_type_body(&mut self) -> Result<TypeDefinitionBody> {
        match self.current_token() {
            // struct
            Token::Identifier(_) => {
                let fields = self.delimited(|this| {
                    let field_name = this.parse_ident()?;
                    this.expect(Token::Colon, "a colon separating the field name from its type");
                    let field_type = this.parse_type()?;
                    Ok((field_name, field_type))
                }, Token::Comma, true);
                Ok(TypeDefinitionBody::Struct(fields))
            }
            // enum
            _ => {
                let variants = self.many0(|this| {
                    this.expect(Token::Pipe, "`|`");
                    let variant_name = this.parse_type_name()?;
                    let parameters = this.many0(Self::parse_type); // TODO: arg type
                    Ok((variant_name, parameters))
                });
                Ok(TypeDefinitionBody::Enum(variants))
            }
        }
    }

    /// Parse an indented block using the given failable parser.
    /// On failure recovers to the unindent token and returns T::error_default.
    fn parse_indented<T>(&mut self, parser: impl FnOnce(&mut Self) -> Result<T>) -> T
        where T: ErrorDefault
    {
        self.expect(Token::Indent, "an indent");

        let result = parser(self);
        if result.is_err() {
            self.recover_to(Token::Unindent, &[]);
        }

        if !self.expect(Token::Unindent, "an unindent") {
            // If we stopped short of the unindent, skip everything until the unindent
            self.recover_to(Token::Unindent, &[]);
            self.advance();
        }

        result.unwrap_or(T::error_default())
    }

    fn parse_type(&mut self) -> Result<Type> {
        match self.current_token() {
            Token::UnitType => {
                self.advance();
                Ok(Type::Unit)
            }
            Token::IntegerType(kind) => {
                self.advance();
                Ok(Type::Integer(*kind))
            }
            _ => {
                let path = self.parse_type_path()?;
                Ok(Type::Named(path))
            }
        }
    }

    fn parse_type_name(&mut self) -> Result<String> {
        match self.current_token() {
            Token::TypeName(name) => {
                self.advance();
                Ok(name.clone())
            }
            other => {
                let actual = other.clone();
                let message = "a capitalized type name";
                let location = self.current_token_location();
                Err(Diagnostic::ParserExpected { message, actual, location })
            }
        }
    }

    fn parse_ident(&mut self) -> Result<String> {
        match self.current_token() {
            Token::Identifier(name) => {
                self.advance();
                Ok(name.clone())
            }
            other => {
                let actual = other.clone();
                let message = "an identifier";
                let location = self.current_token_location();
                Err(Diagnostic::ParserExpected { message, actual, location })
            }
        }
    }

    /// Parse 0 or more of `parser` items
    fn many0<T>(&mut self, mut parser: impl FnMut(&mut Self) -> Result<T>) -> Vec<T> {
        let mut items = Vec::new();
        while let Ok(item) = parser(self) {
            items.push(item);
        }
        items
    }

    /// Parse 1 or more of `parser` items
    fn many1<T>(&mut self, mut parser: impl FnMut(&mut Self) -> Result<T>) -> Result<Vec<T>> {
        let mut items = vec![parser(self)?];
        while let Ok(item) = parser(self) {
            items.push(item);
        }
        Ok(items)
    }

    fn delimited<T>(&mut self,
        mut parser: impl FnMut(&mut Self) -> Result<T>,
        delimiter: Token,
        allow_trailing: bool,
    ) -> Vec<T> {
        let mut items = Vec::new();

        match parser(self) {
            Ok(item) => items.push(item),
            Err(_) => return items,
        }

        while self.accept(delimiter.clone()) {
            match parser(self) {
                Ok(item) => items.push(item),
                Err(_) if allow_trailing => break,
                Err(error) => {
                    self.diagnostics.push(error);
                    break;
                }
            }
        }

        items
    }

    fn parse_function_parameters(&mut self) -> Vec<(String, Option<Type>)> {
        self.many0(Self::parse_function_parameter)
    }

    fn parse_function_parameter(&mut self) -> Result<(String, Option<Type>)> {
        match self.current_token() {
            Token::UnitLiteral => {
                // TODO: Remove 'no name' hack for unit literal parameter
                self.advance();
                let no_name = String::new();
                Ok((no_name, Some(Type::Unit)))
            }
            Token::ParenthesisLeft => {
                self.advance();
                let name = self.parse_ident()?;
                self.expect(Token::Colon, "a colon to specify the type of this parameter");

                let typ = self.parse_with_recovery(Self::parse_type, Token::ParenthesisRight, &[Token::Newline, Token::Equal])?;

                self.expect(Token::ParenthesisRight, "a `)` to close the opening `(` from the parameter");
                Ok((name, Some(typ)))
            }
            Token::Identifier(_) => Ok((self.parse_ident()?, None)),
            other => {
                let message = "a parameter";
                let location = self.current_token_location();
                Err(Diagnostic::ParserExpected { message, actual: other.clone(), location })
            }
        }
    }

    /// Returns the precedence of an operator along with
    /// whether or not it is right-associative.
    /// Returns None if the given Token is not an operator
    fn precedence(token: &Token) -> Option<(i8, bool)> {
        match token {
            Token::Semicolon => Some((0, false)),
            Token::ApplyRight => Some((1, false)),
            Token::ApplyLeft => Some((2, true)),
            Token::Comma => Some((3, true)),
            Token::Or => Some((4, false)),
            Token::And => Some((5, false)),
            Token::EqualEqual
            | Token::NotEqual
            | Token::GreaterThan
            | Token::LessThan
            | Token::GreaterThanOrEqual
            | Token::LessThanOrEqual => Some((7, false)),
            Token::In => Some((8, false)),
            Token::Append => Some((9, false)),
            Token::Range => Some((10, false)),
            Token::Add | Token::Subtract => Some((11, false)),
            Token::Multiply | Token::Divide | Token::Modulus => Some((12, false)),
            Token::Index => Some((14, false)),
            Token::As => Some((15, false)),
            _ => None,
        }
    }

    /// Should we push this operator onto our operator stack and keep parsing our expression?
    /// This handles the operator precedence and associativity parts of the shunting-yard algorithm.
    fn should_continue(operator_on_stack: &Token, r_prec: i8, r_is_right_assoc: bool) -> bool {
        let (l_prec, _) = Self::precedence(operator_on_stack).unwrap();

        l_prec > r_prec || (l_prec == r_prec && !r_is_right_assoc)
    }

    fn pop_operator<'c>(&mut self, operator_stack: &mut Vec<&(Token, Span)>, results: &mut Vec<ExprId>) {
        let rhs = results.pop().unwrap();
        let lhs = results.pop().unwrap();
        let location = self.expr_location(lhs).to(&self.expr_location(rhs));

        let call = self.reserve_expr();
        let function = self.reserve_expr();

        let (operator, span) = operator_stack.pop().unwrap().clone();
        let function_location = span.in_file(self.file_path.clone());

        let components = vec![operator.to_string()]; // TODO: Variable::operator
        self.insert_expr(function, Expr::Variable(Path { components }), function_location);

        let call_expr = Expr::Call(Call { function, arguments: vec![lhs, rhs] });
        self.insert_expr(call, call_expr, location);
        results.push(call);
    }

    /// Parse an arbitrary expression using the shunting-yard algorithm
    fn parse_expression(&mut self) -> Result<ExprId> {
        let value = self.parse_term()?;

        let mut operator_stack: Vec<&(Token, Span)> = vec![];
        let mut results = vec![value];

        // loop while the next token is an operator
        while let Some((prec, right_associative)) = Self::precedence(self.current_token()) {
            while !operator_stack.is_empty()
                && Self::should_continue(&operator_stack[operator_stack.len() - 1].0, prec, right_associative)
            {
                self.pop_operator(&mut operator_stack, &mut results);
            }

            operator_stack.push(self.current_token_and_span());
            self.next_token();

            let value = self.parse_term()?;
            results.push(value);
        }

        while !operator_stack.is_empty() {
            assert!(results.len() >= 2);
            self.pop_operator(&mut operator_stack, &mut results);
        }

        assert!(operator_stack.is_empty());
        assert!(results.len() == 1);
        Ok(results.pop().unwrap())
    }

    fn parse_term(&mut self) -> Result<ExprId> {
        match self.current_token() {
            // definition, variable, function call
            Token::Identifier(_) | Token::TypeName(_) => {
                if let Ok(call) = self.parse_function_call() {
                    return Ok(call);
                }

                self.parse_atom()
            }
            Token::Subtract | Token::Ampersand | Token::ExclamationMark | Token::At => {
                self.parse_unary()
            }
            _ => self.parse_atom(),
        }
    }

    fn parse_unary(&mut self) -> Result<ExprId> {
        match self.current_token() {
            operator @ (Token::Subtract | Token::ExclamationMark | Token::Ampersand | Token::At) => {
                let call_id = self.reserve_expr();
                let function_id = self.reserve_expr();

                let operator_span = self.current_token_location();
                self.advance();
                let rhs = self.parse_unary()?;
                let location = operator_span.to(&self.expr_location(rhs));

                let components = vec![operator.to_string()];
                self.insert_expr(function_id, Expr::Variable(Path { components }), location.clone());

                let call = Expr::Call(Call { function: function_id, arguments: vec![rhs] });
                self.insert_expr(call_id, call, location);
                Ok(call_id)
            }
            _ => self.parse_atom(),
        }
    }

    /// Very similar to `parse_unary` but excludes unary minus since otherwise
    /// we may parse `{function_name} -{arg}` instead of `{lhs} - {rhs}`.
    fn parse_function_arg(&mut self) -> Result<ExprId> {
        match self.current_token() {
            Token::At => {
                self.with_expr_id_and_location(|this| {
                    let operator_location = this.current_token_location();
                    this.advance();
                    let rhs = this.parse_unary()?;
                    let components = vec![Token::At.to_string()];
                    let function = Expr::Variable(Path { components });
                    let function = this.push_expr(function, operator_location);
                    Ok(Expr::Call(Call { function, arguments: vec![rhs] }))
                })
            }
            operator @ (Token::ExclamationMark | Token::Ampersand) => {
                let mode = match operator {
                    Token::ExclamationMark => BorrowMode::Mutable(SharedMode::Shared),
                    Token::Ampersand => BorrowMode::Immutable(SharedMode::Shared),
                    _ => unreachable!(),
                };

                self.with_expr_id_and_location(|this| {
                    this.advance();
                    let rhs = this.parse_unary()?;
                    Ok(Expr::Reference(cst::Reference { mode, rhs }))
                })
            }
            _ => self.parse_atom(),
        }
    }

    /// An atom is a very small unit of parsing, but one that can still be divided further.
    /// In this case it is made up of quarks connected by `.` or unary expressions
    fn parse_atom(&mut self) -> Result<ExprId> {
        let mut result = self.parse_quark()?;

        loop {
            let token = self.current_token();
            match token {
                Token::MemberAccess
                | Token::MemberRef
                | Token::MemberMut => {
                    result = self.with_expr_id_and_location(|this| {
                        this.advance();
                        let ownership = OwnershipMode::from_token(token).unwrap();
                        let member = this.parse_ident()?;
                        Ok(Expr::MemberAccess(MemberAccess { object: result, member, ownership }))
                    })?;
                },
                Token::Index
                | Token::IndexRef
                | Token::IndexMut => {
                    result = self.with_expr_id_and_location(|this| {
                        this.advance();
                        let ownership = OwnershipMode::from_token(token).unwrap();
                        let index = this.parse_expression()?;
                        this.try_expect(Token::BracketRight, "a `]` to terminate the index expression")?;
                        Ok(Expr::Index(Index { object: result, index, ownership }))
                    })?;
                },
                _ => break Ok(result),
            }
        }
    }

    fn parse_quark(&mut self) -> Result<ExprId> {
        match self.current_token() {
            Token::IntegerLiteral(value, kind) => {
                let (value, kind) = (*value, *kind);
                let location = self.current_token_location();
                self.advance();
                let expr = Expr::Literal(Literal::Integer(value, kind));
                Ok(self.push_expr(expr, location))
            }
            Token::StringLiteral(s) => self.parse_string(s.clone()),
            Token::Identifier(_) | Token::TypeName(_) => self.parse_variable(),
            Token::Indent => {
                let (expr, location) = self.with_location(|this| {
                    Ok(this.parse_indented(|this| {
                        let statements = this.delimited(Self::parse_sequence_item, Token::Newline, true);
                        Ok(Expr::Sequence(statements))
                    }))
                })?;
                Ok(self.push_expr(expr, location))
            }
            other => {
                let message = "an expression";
                let location= self.current_token_location();
                Err(Diagnostic::ParserExpected { message, actual: other.clone(), location})
            }
        }
    }

    fn parse_sequence_item(&mut self) -> Result<SequenceItem> {
        let comments = self.parse_comments();
        let expr = self.parse_statement()?;
        Ok(SequenceItem { comments, expr })
    }

    fn parse_statement(&mut self) -> Result<ExprId> {
        todo!()
    }

    fn with_expr_id(&mut self, f: impl FnOnce(&mut Self) -> Result<(Expr, Location)>) -> Result<ExprId> {
        let id = self.reserve_expr();
        let (expr, location) = f(self)?;
        self.insert_expr(id, expr, location);
        Ok(id)
    }

    fn with_expr_id_and_location(&mut self, f: impl FnOnce(&mut Self) -> Result<Expr>) -> Result<ExprId> {
        self.with_expr_id(|this| this.with_location(f))
    }

    /// Create a location from the current token before running the given parse function to the
    /// current token (end exclusive) after running the given parse function.
    fn with_location<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<(T, Location)> {
        let start = self.current_token_span();
        let ret = f(self)?;
        let end = self.previous_token_span();
        Ok((ret, start.to(&end).in_file(self.file_path.clone())))
    }

    fn parse_variable(&mut self) -> Result<ExprId> {
        let (path, location) = self.with_location(|this| this.parse_value_path())?;
        Ok(self.push_expr(Expr::Variable(path), location))
    }

    fn parse_function_call(&mut self) -> Result<ExprId> {
        self.with_expr_id(|this| {
            let function = this.parse_atom()?;
            let arguments = this.many1(Self::parse_function_arg)?;

            let last_arg_location = this.expr_location(*arguments.last().unwrap());
            let location = this.expr_location(function).to(&last_arg_location);

            Ok((Expr::Call(Call { function, arguments }), location))
        })
    }

    fn parse_string(&mut self, contents: String) -> Result<ExprId> {
        let location = self.current_token_location();
        self.advance();
        Ok(self.push_expr(Expr::Literal(Literal::String(contents)), location))
    }
}
