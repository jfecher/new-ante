use crate::{error::{CompileResult, Span, Diagnostic}, lexer::{Lexer, token::Token}};

use self::cst::{Cst, Import, Path, Ident, TopLevelItem, Methods, TopLevelItemKind, TypeDefinition, Function};

mod cst;

type ParseResult<T> = Result<T, Diagnostic>;

struct Parser {
    tokens: Vec<(Token, Span)>,
    warnings: Vec<Diagnostic>,
    errors: Vec<Diagnostic>,

    token_index: usize,
}

pub fn parse_file(file_contents: &str) -> CompileResult<Cst> {
    let tokens = Lexer::new(&file_contents).collect();
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

    fn current_token_and_span(&self) -> &(Token, Span) {
        &self.tokens[self.token_index]
    }

    fn advance(&mut self) {
        self.token_index += 1;
    }

    fn accept(&mut self, token: Token) -> bool {
        if *self.current_token() == token {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, token: Token, message: &'static str) {
        if !self.accept(token) {
            let actual = self.current_token().clone();
            self.errors.push(Diagnostic::ExpectedToken { message, actual })
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
                let message = "an identifier";
                Err(Diagnostic::ExpectedToken { message, actual })
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
            Token::Identifier(_) => TopLevelItemKind::FunctionGroup(vec![self.parse_function()]),
            Token::Type => TopLevelItemKind::TypeDefinition(self.parse_type_definition()),
            Token::Methods => TopLevelItemKind::Methods(self.parse_methods()),
            _ => {
                todo!()
                // self.errors.push(Diagnostic::ExpectedToken { message: (), actual: () });
            }
        };

        Ok(TopLevelItem { comments, kind })
    }

    fn parse_comments(&mut self) -> Vec<String> {
        let mut comments = Vec::new();

        while let Token::LineComment(comment) = self.current_token() {
            comments.push(comment.clone());
            self.expect(Token::Newline, "newline after comment");
        }

        comments
    }

    fn parse_methods(&mut self) -> Methods {
        self.expect(Token::Methods, "methods"); // expected to be unreachable
        let typ = self.parse_type();
        self.expect(Token::Equal, "`=` after methods declaration");
        let functions = self.parse_functions();
        Methods { typ, functions }
    }

    fn parse_functions(&mut self) -> Vec<Function> {
        self.expect(Token::Methods, "methods");
        todo!()
    }

    fn parse_function(&mut self) -> Function {
        self.expect(Token::Methods, "methods");
        todo!()
    }

    fn parse_type_definition(&mut self) -> TypeDefinition {
        self.expect(Token::Methods, "methods");
        todo!()
    }
}
