use crate::{error::{CompileResult, Span, Diagnostic, ErrorDefault}, lexer::{Lexer, token::Token}};

use self::cst::{Cst, Import, Path, Ident, TopLevelItem, Methods, TopLevelItemKind, TypeDefinition, Function, Type, Expr, TypeDefinitionBody, Literal, SequenceItem, Definition, Call};

mod cst;

type ParseResult<T> = Result<T, Diagnostic>;

struct Parser {
    tokens: Vec<(Token, Span)>,
    warnings: Vec<Diagnostic>,
    errors: Vec<Diagnostic>,

    token_index: usize,
}

pub fn parse_file(file_contents: &str) -> CompileResult<Cst> {
    let tokens = Lexer::new(&file_contents).collect::<Vec<_>>();
    for (token, _) in &tokens {
        eprintln!("{token}");
    }
    Parser::new(tokens).parse()
}

impl Parser {
    fn new(tokens: Vec<(Token, Span)>) -> Self {
        Self {
            tokens,
            warnings: Vec::new(),
            errors: Vec::new(),
            token_index: 0,
        }
    }

    fn parse(mut self) -> CompileResult<Cst> {
        let imports = self.parse_imports();
        let top_level_items = self.parse_top_level_items();
        let item = Cst { imports, top_level_items };
        CompileResult { item, warnings: self.warnings, errors: self.errors }
    }

    fn current_token(&self) -> &Token {
        &self.tokens[self.token_index].0
    }

    /// Returns the previous token, if it exists.
    /// Returns the current token otherwise.
    fn previous_token(&self) -> &Token {
        &self.tokens[self.token_index.saturating_sub(1)].0
    }

    /// Returns the next token.
    /// Panics if the current token is EOF
    fn next_token(&self) -> &Token {
        &self.tokens[self.token_index + 1].0
    }

    fn current_token_and_span(&self) -> &(Token, Span) {
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
        if self.accept(token) {
            true
        } else {
            let actual = self.current_token().clone();
            self.errors.push(Diagnostic::ParserExpected { message, actual });
            false
        }
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
    fn try_parse_or_recover_to_newline<T>(&mut self, parser: impl FnOnce(&mut Self) -> ParseResult<T>) -> Option<T> {
        match parser(self) {
            Ok(item) => Some(item),
            Err(error) => {
                self.errors.push(error);
                self.recover_to_next_newline();
                None
            }
        }
    }

    fn parse_imports(&mut self) -> Vec<Import> {
        let mut imports = Vec::new();

        while self.accept(Token::Import) {
            if let Some(path) = self.try_parse_or_recover_to_newline(Self::parse_path) {
                imports.push(Import { path });
            }
            self.expect(Token::Newline, "newline after import");
        }

        imports
    }

    fn parse_path(&mut self) -> ParseResult<Path> {
        let mut components = vec![self.parse_ident_or_typename()?];

        while self.accept(Token::MemberAccess) {
            components.push(self.parse_ident_or_typename()?);
        }

        Ok(Path { components })
    }

    fn parse_ident_or_typename(&mut self) -> ParseResult<Ident> {
        match self.current_token_and_span() {
            (Token::Identifier(name) | Token::TypeName(name), span) => {
                let (name, span) = (name.clone(), *span);
                self.advance();
                Ok(Ident::new(name, span))
            }
            (other, _) => {
                let actual = other.clone();
                let message = "an identifier or type name";
                Err(Diagnostic::ParserExpected { message, actual })
            }
        }
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

    fn parse_top_level_item(&mut self) -> ParseResult<TopLevelItem> {
        let comments = self.parse_comments();

        let kind = match self.current_token() {
            Token::Identifier(_) => {
                let functions = self.parse_functions();
                // parse_functions can eat the trailing newline
                if *self.previous_token() == Token::Newline {
                    self.token_index -= 1;
                }
                TopLevelItemKind::FunctionGroup(functions)
            }
            Token::Type => TopLevelItemKind::TypeDefinition(self.parse_type_definition()?),
            Token::Methods => TopLevelItemKind::Methods(self.parse_methods()?),
            other => {
                let message = "a top-level item";
                return Err(Diagnostic::ParserExpected { message, actual: other.clone() });
            }
        };

        Ok(TopLevelItem { comments, kind })
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

    fn parse_methods(&mut self) -> ParseResult<Methods> {
        self.expect(Token::Methods, "`methods`"); // expected to be unreachable

        let typ = match self.parse_type() {
            Ok(typ) => typ,
            Err(error) => {
                if self.recover_to(Token::Equal, &[Token::Newline]) {
                    self.errors.push(error);
                    Type::Error
                } else {
                    return Err(error);
                }
            }
        };

        self.expect(Token::Equal, "`=` after methods declaration");
        self.expect(Token::Indent, "indent after `=` to begin the methods list");
        let functions = self.parse_functions();
        self.expect(Token::Unindent, "unindent to end the methods list");
        Ok(Methods { typ, functions })
    }

    fn parse_functions(&mut self) -> Vec<Function> {
        self.delimited(Self::parse_function, Token::Newline, true)
    }

    fn parse_function(&mut self) -> ParseResult<Function> {
        let name = self.parse_ident()?;
        let parameters = self.parse_function_parameters()?;

        let mut return_type = None;
        if self.accept(Token::Colon) {
            return_type = self.parse_with_recovery(Self::parse_type, Token::Equal, &[Token::Newline, Token::Indent]).ok();
        }
        self.expect(Token::Equal, "`=` to begin the function body");

        let body = self.parse_expr().map_err(|error| self.errors.push(error)).ok();
        Ok(Function { name, parameters, return_type, body })
    }

    fn parse_type_definition(&mut self) -> ParseResult<TypeDefinition> {
        self.expect(Token::Type, "`type`");
        let name = self.parse_type_name()?;
        self.expect(Token::Equal, "`=` to begin the type definition");
        let body = self.parse_type_body()?;
        Ok(TypeDefinition { name, body })
    }

    fn parse_type_body(&mut self) -> ParseResult<TypeDefinitionBody> {
        match self.current_token() {
            Token::Indent => self.parse_indented(Self::parse_indented_type_body),
            _ => self.parse_non_indented_type_body(),
        }
    }

    fn parse_indented_type_body(&mut self) -> ParseResult<TypeDefinitionBody> {
        match self.current_token() {
            // struct
            Token::Identifier(_) => {
                let fields = self.delimited(|this| {
                    this.expect(Token::Pipe, "`|`");
                    let field_name = this.parse_type_name()?;
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

    fn parse_non_indented_type_body(&mut self) -> ParseResult<TypeDefinitionBody> {
        match self.current_token() {
            // struct
            Token::Identifier(_) => {
                let fields = self.delimited(|this| {
                    this.expect(Token::Pipe, "`|`");
                    let field_name = this.parse_type_name()?;
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
    /// On failure recovers to the unindent token.
    fn parse_indented<T>(&mut self, parser: impl FnOnce(&mut Self) -> ParseResult<T>) -> ParseResult<T>
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

        result
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        match self.current_token() {
            Token::UnitType => {
                self.advance();
                Ok(Type::Unit)
            }
            _ => {
                let path = self.parse_path()?;
                Ok(Type::Named(path))
            }
        }
    }

    fn parse_type_name(&mut self) -> ParseResult<Ident> {
        match self.current_token_and_span() {
            (Token::TypeName(name), span) => {
                let (name, span) = (name.clone(), *span);
                self.advance();
                Ok(Ident::new(name, span))
            }
            (other, _) => {
                let actual = other.clone();
                let message = "a capitalized type name";
                Err(Diagnostic::ParserExpected { message, actual })
            }
        }
    }

    fn parse_ident(&mut self) -> ParseResult<Ident> {
        match self.current_token_and_span() {
            (Token::Identifier(name), span) => {
                let (name, span) = (name.clone(), *span);
                self.advance();
                Ok(Ident::new(name, span))
            }
            (other, _) => {
                let actual = other.clone();
                let message = "an identifier";
                Err(Diagnostic::ParserExpected { message, actual })
            }
        }
    }

    /// Parse 0 or more of `parser` items
    fn many0<T>(&mut self, mut parser: impl FnMut(&mut Self) -> ParseResult<T>) -> Vec<T> {
        let mut items = Vec::new();
        while let Ok(item) = parser(self) {
            items.push(item);
        }
        items
    }

    /// Parse 1 or more of `parser` items
    fn many1<T>(&mut self, mut parser: impl FnMut(&mut Self) -> ParseResult<T>) -> ParseResult<Vec<T>> {
        let mut items = vec![parser(self)?];
        while let Ok(item) = parser(self) {
            items.push(item);
        }
        Ok(items)
    }

    fn delimited<T>(&mut self,
        mut parser: impl FnMut(&mut Self) -> ParseResult<T>,
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
                    self.errors.push(error);
                    break;
                }
            }
        }

        items
    }

    fn parse_function_parameters(&mut self) -> ParseResult<Vec<(Ident, Option<Type>)>> {
        self.many1(Self::parse_function_parameter)
    }

    /// Run the given parse function and return its result on success.
    ///
    /// On error, try to recover to the given token, stopping short if any of
    /// the `too_far` tokens (or EOF) are found first. On a successful recovery,
    /// return the given default error value. Otherwise return the original error.
    fn parse_with_recovery<T>(&mut self, f: impl FnOnce(&mut Self) -> ParseResult<T>, recover_to: Token, too_far: &[Token]) -> ParseResult<T>
        where T: ErrorDefault
    {
        match f(self) {
            Ok(typ) => Ok(typ),
            Err(error) => {
                if self.recover_to(recover_to, too_far) {
                    self.errors.push(error);
                    Ok(T::error_default())
                } else {
                    Err(error)
                }
            }
        }
    }

    fn parse_function_parameter(&mut self) -> ParseResult<(Ident, Option<Type>)> {
        match self.current_token() {
            Token::UnitLiteral => {
                // TODO: Remove 'no name' hack for unit literal parameter
                self.advance();
                let span_start = self.tokens[self.token_index - 2].1;
                let span_end = self.tokens[self.token_index - 1].1;
                let no_name = Ident::new(String::new(), span_start.merge(span_end));
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
                Err(Diagnostic::ParserExpected { message, actual: other.clone() })
            }
        }
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        match self.current_token() {
            Token::IntegerLiteral(value, kind) => {
                let (value, kind) = (*value, *kind);
                self.advance();
                Ok(Expr::Literal(Literal::Integer(value, kind)))
            }
            Token::StringLiteral(s) => self.parse_string(s),
            // definition or variable
            Token::Identifier(_) => {
                match self.next_token() {
                    Token::Equal => self.parse_definition(),
                    _ => {
                        if let Ok(call) = self.parse_function_call() {
                            return Ok(call);
                        }

                        let path = self.parse_path()?;
                        Ok(Expr::Variable(path))
                    }
                }
            }
            Token::Indent => {
                let statements = self.parse_indented(|this| {
                    Ok(this.delimited(|this| {
                        let comments = this.parse_comments();
                        let expr = this.parse_expr()?;
                        Ok(SequenceItem { comments, expr })
                    }, Token::Indent, true))
                })?;
                Ok(Expr::Sequence(statements))
            }
            other => {
                let message = "an expression";
                self.errors.push(Diagnostic::ParserExpected { message, actual: other.clone() });
                Ok(Expr::Error)
            }
        }
    }

    fn parse_atom(&mut self) -> ParseResult<Expr> {
        match self.current_token() {
            Token::IntegerLiteral(value, kind) => {
                let (value, kind) = (*value, *kind);
                self.advance();
                Ok(Expr::Literal(Literal::Integer(value, kind)))
            }
            Token::StringLiteral(s) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::Literal(Literal::String(s.clone())))
            }
            // definition or variable
            Token::Identifier(_) => {
                let path = self.parse_path()?;
                Ok(Expr::Variable(path))
            }
            Token::Indent => {
                let statements = self.parse_indented(|this| {
                    Ok(this.delimited(|this| {
                        let comments = this.parse_comments();
                        let expr = this.parse_expr()?;
                        Ok(SequenceItem { comments, expr })
                    }, Token::Indent, true))
                })?;
                Ok(Expr::Sequence(statements))
            }
            other => {
                let message = "an expression";
                Err(Diagnostic::ParserExpected { message, actual: other.clone() })
            }
        }
    }

    fn parse_definition(&mut self) -> ParseResult<Expr> {
        let name = self.parse_ident()?;

        let mut typ = None;
        if self.accept(Token::Colon) {
            match self.parse_with_recovery(Self::parse_type, Token::Equal, &[Token::Newline]) {
                Ok(recovered) => typ = Some(recovered),
                Err(error) => {
                    self.errors.push(error);
                    return Ok(Expr::Error);
                }
            }
        }

        self.expect(Token::Equal, "`=` to define a variable");

        let rhs = self.parse_with_recovery(Self::parse_expr, Token::Newline, &[])?;

        Ok(Expr::Definition(Definition { name, typ, rhs: Box::new(rhs) }))
    }

    fn parse_function_call(&mut self) -> ParseResult<Expr> {
        let function = Box::new(self.parse_atom()?);
        let arguments = self.many1(Self::parse_atom)?;
        Ok(Expr::Call(Call { function, arguments }))
    }

    fn parse_string(&mut self, contents: &str) -> ParseResult<Expr> {
        self.advance();

        let mut contents = contents.to_string();

        while self.accept(Token::InterpolateLeft) {

        }

        Ok(Expr::Literal(Literal::String(contents)))
    }
}
