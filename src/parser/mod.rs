use std::{collections::BTreeMap, sync::Arc};

use cst::{BorrowMode, Comptime, DefinitionName, Index, Lambda, MemberAccess, Name, OwnershipMode, Pattern, SharedMode};
use ids::{ExprId, NameId, PathId, PatternId, TopLevelId};
use rustc_hash::FxHashSet;
use serde::{Deserialize, Serialize};

use crate::{diagnostics::{Diagnostic, ErrorDefault, Location, LocationData, Span}, incremental, lexer::{token::Token, Lexer}, name_resolution::namespace::SourceFileId, vecmap::VecMap};

use self::cst::{Cst, Import, Path, TopLevelItem, TopLevelItemKind, TypeDefinition, Type, Expr, TypeDefinitionBody, Literal, SequenceItem, Definition, Call};

pub mod cst;
pub mod cst_printer;
pub mod ids;

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ParseResult {
    pub cst: Cst,
    pub diagnostics: Vec<Diagnostic>,
    pub top_level_data: BTreeMap<TopLevelId, Arc<TopLevelContext>>,
}

/// Metadata associated with a top level statement
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct TopLevelContext {
    pub location: Location,
    pub exprs: VecMap<ExprId, Expr>,
    pub patterns: VecMap<PatternId, Pattern>,
    pub paths: VecMap<PathId, Path>,
    pub names: VecMap<NameId, Name>,

    pub expr_locations: VecMap<ExprId, Location>,
    pub pattern_locations: VecMap<PatternId, Location>,
    pub path_locations: VecMap<PathId, Location>,
    pub name_locations: VecMap<NameId, Location>,
}

impl TopLevelContext {
    fn new(file_id: SourceFileId) -> Self {
        Self {
            location: LocationData::placeholder(file_id),
            exprs: VecMap::default(),
            patterns: VecMap::default(),
            expr_locations: VecMap::default(),
            pattern_locations: VecMap::default(),
            paths: VecMap::default(),
            names: VecMap::default(),
            path_locations: VecMap::default(),
            name_locations: VecMap::default(),
        }
    }
}

type Result<T> = std::result::Result<T, Diagnostic>;

struct Parser<'tokens> {
    file_id: SourceFileId,
    tokens: &'tokens [(Token, Span)],
    diagnostics: Vec<Diagnostic>,
    top_level_data: BTreeMap<TopLevelId, Arc<TopLevelContext>>,

    /// Keep track of any name collisions in the top level items
    top_level_item_hashes: FxHashSet<u64>,

    current_context: TopLevelContext,

    token_index: usize,
}

pub fn parse_impl(ctx: &incremental::Parse, db: &incremental::DbHandle) -> Arc<ParseResult> {
    let file = ctx.0.get(db);
    let tokens = Lexer::new(&file.contents).collect::<Vec<_>>();
    Arc::new(Parser::new(ctx.0, &tokens).parse())
}

impl<'tokens> Parser<'tokens> {
    fn new(file_id: SourceFileId, tokens: &'tokens [(Token, Span)]) -> Self {
        Self {
            file_id,
            tokens,
            diagnostics: Vec::new(),
            token_index: 0,
            top_level_data: Default::default(),
            top_level_item_hashes: Default::default(),
            current_context: TopLevelContext::new(file_id),
        }
    }

    fn parse(mut self) -> ParseResult {
        let imports = self.parse_imports();
        let top_level_items = self.parse_top_level_items();
        let ending_comments = self.parse_comments();
        let cst = Cst { imports, top_level_items, ending_comments };
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
        self.current_token_span().in_file(self.file_id)
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

    /// Returns the previous token's location, if it exists.
    /// Returns the current token's location otherwise.
    fn previous_token_location(&self) -> Location {
        self.previous_token_span().in_file(self.file_id)
    }

    fn current_token_and_span(&self) -> &'tokens (Token, Span) {
        &self.tokens[self.token_index]
    }

    fn advance(&mut self) {
        self.token_index = self.tokens.len().min(self.token_index + 1);
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
    fn expect(&mut self, token: Token, message: &'static str) -> Result<()> {
        if self.accept(token) {
            Ok(())
        } else {
            self.expected(message)
        }
    }

    /// Return a `ParserExpected` error.
    /// Uses the current token as the actual token for comparison and for the location for the error.
    fn expected<T>(&self, message: impl Into<String>) -> Result<T> {
        let message = message.into();
        let actual = self.current_token().clone();
        let location = self.current_token_location();
        Err(Diagnostic::ParserExpected { message, actual, location })
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

    fn reserve_pattern(&mut self) -> PatternId {
        let id = self.current_context.patterns.push(Pattern::Error);
        let id2 = self.current_context.pattern_locations.push(self.current_token_location());
        assert_eq!(id, id2);
        id
    }

    fn insert_pattern(&mut self, id: PatternId, pattern: Pattern, location: Location) {
        self.current_context.patterns[id] = pattern;
        self.current_context.pattern_locations[id] = location;
    }

    fn push_pattern(&mut self, pattern: Pattern, location: Location) -> PatternId {
        let id = self.current_context.patterns.push(pattern);
        let id2 = self.current_context.pattern_locations.push(location);
        assert_eq!(id, id2);
        id
    }

    fn push_path(&mut self, path: Path, location: Location) -> PathId {
        let id = self.current_context.paths.push(path);
        let id2 = self.current_context.path_locations.push(location);
        assert_eq!(id, id2);
        id
    }

    fn push_name(&mut self, name: Name, location: Location) -> NameId {
        let id = self.current_context.names.push(name);
        let id2 = self.current_context.name_locations.push(location);
        assert_eq!(id, id2);
        id
    }

    /// Return a hash of the given data guaranteed to be unique within the current module.
    fn hash_top_level_data(&mut self, data: &impl std::hash::Hash) -> u64 {
        for collisions in 0.. {
            let hash = ids::hash((data, collisions));
            if self.top_level_item_hashes.insert(hash) {
                return hash;
            }
        }
        unreachable!()
    }

    /// Create a new TopLevelId from the name of a given top level item.
    /// In the case of definitions, this name will be only the last element in their path.
    fn new_top_level_id(&mut self, data: impl std::hash::Hash) -> TopLevelId {
        // Check for previous name collisions to disambiguate the resulting hash
        let hash = self.hash_top_level_data(&data);

        let id = TopLevelId::new(self.file_id, hash);
        let empty_context = TopLevelContext::new(self.file_id);
        let old_context = std::mem::replace(&mut self.current_context, empty_context);
        self.top_level_data.insert(id.clone(), Arc::new(old_context));
        id
    }

    /// Return the location of an ExprId within the current context
    fn expr_location(&self, expr: ExprId) -> Location {
        self.current_context.expr_locations[expr].clone()
    }

    /// Skip all tokens up to the next newline (or unindent) token.
    /// If an Indent token is encountered we'll try to match indents and unindents
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
                Token::EndOfInput => return,
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

    /// Same as `parse_with_expr` but recovers with `Expr::Error` with an approximated location
    /// since `ExprId` does not implement `ErrorDefault`.
    fn parse_expr_with_recovery(&mut self, f: impl FnOnce(&mut Self) -> Result<ExprId>, recover_to: Token, too_far: &[Token]) -> Result<ExprId> {
        match f(self) {
            Ok(typ) => Ok(typ),
            Err(error) => {
                let start = self.current_token_span();
                if self.recover_to(recover_to, too_far) {
                    self.diagnostics.push(error);
                    let end = self.current_token_span();
                    let location = start.to(&end).in_file(self.file_id);
                    let expr = self.push_expr(Expr::Error, location);
                    Ok(expr)
                } else {
                    Err(error)
                }
            }
        }
    }

    fn parse_imports(&mut self) -> Vec<Import> {
        let mut imports = Vec::new();
        self.accept(Token::Newline);

        loop {
            let position_before_comments = self.token_index;
            let comments = self.parse_comments();

            if !self.accept(Token::Import) {
                // The comments, if any, should be attached to the next top level item
                // since there is no import here.
                self.token_index = position_before_comments;
                break;
            }

            let start = self.current_token_span();
            if let Some(mut path) = self.try_parse_or_recover_to_newline(Self::parse_type_or_value_path) {
                let mut items = Vec::with_capacity(1);
                if let Some(item) = path.components.pop() {
                    items.push(item);
                }

                // Parse any extra items `, b, c, d`
                while self.accept(Token::Comma) {
                    match self.parse_ident() {
                        Ok(name) => items.push((name, self.previous_token_location())),
                        Err(error) => self.diagnostics.push(error),
                    }
                }

                let end = self.previous_token_span();
                let location = start.to(&end).in_file(self.file_id);
                let path = path.into_file_path();
                imports.push(Import { comments, module_path: path, items, location });
            }

            self.expect_newline_with_recovery("a newline after the import");
        }

        imports
    }

    fn expect_newline_with_recovery(&mut self, error_message: &'static str) {
        let expect_newline = |this: &mut Self| this.expect(Token::Newline, error_message);
        if self.try_parse_or_recover_to_newline(expect_newline).is_none() {
            // We should have recovered to a newline by this point, so we need to parse it again.
            // Don't error here, the only errors possible are if we recover to an Unindent or
            // the end of
            // the file.
            expect_newline(self).ok();
        }
    }

    // value_path: (typename '.')* ident
    fn parse_value_path(&mut self) -> Result<Path> {
        let mut components = Vec::new();

        while let Ok(typename) = self.parse_type_name() {
            let location = self.previous_token_location();
            components.push((typename, location));
            self.expect(Token::MemberAccess, "`.` after module name")?;
        }

        let location = self.current_token_location();
        components.push((self.parse_ident()?, location));
        Ok(Path { components })
    }

    // type_path: (typename '.')* typename
    fn parse_type_path(&mut self) -> Result<Path> {
        let location = self.current_token_location();
        let mut components = vec![(self.parse_type_name()?, location)];

        while self.accept(Token::MemberAccess) {
            let location = self.current_token_location();
            components.push((self.parse_type_name()?, location));
        }

        Ok(Path { components })
    }

    // type_or_value_path: (typename '.')* (ident | typename)
    fn parse_type_or_value_path(&mut self) -> Result<Path> {
        let mut components = Vec::new();

        while let Ok(typename) = self.parse_type_name() {
            let location = self.previous_token_location();
            components.push((typename, location));

            if !self.accept(Token::MemberAccess) {
                return Ok(Path { components });
            }
        }

        // If we made it here we had a trailing `.` but the token after it was not a typename,
        // so it must be a variable name.
        let location = self.current_token_location();
        components.push((self.parse_ident()?, location));
        Ok(Path { components })
    }

    fn parse_top_level_items(&mut self) -> Vec<Arc<TopLevelItem>> {
        let mut items = Vec::new();

        while *self.current_token() != Token::EndOfInput {
            let position_before_comments = self.token_index;
            let comments = self.parse_comments();

            // We may have comments at the end of the file not attached to any top level item
            if *self.current_token() == Token::EndOfInput {
                self.token_index = position_before_comments;
                return items;
            }

            if let Some(item) = self.try_parse_or_recover_to_newline(|this| this.parse_top_level_item(comments)) {
                items.push(Arc::new(item));
            }

            // In case there is no newline at the end of the file
            if *self.current_token() == Token::EndOfInput {
                break;
            }
            self.expect_newline_with_recovery("a newline after the top level item");
        }

        items
    }

    fn parse_top_level_item(&mut self, comments: Vec<String>) -> Result<TopLevelItem> {
        let id: TopLevelId;

        let kind = match self.current_token() {
            Token::Identifier(_) | Token::TypeName(_) => {
                let definition = self.parse_definition()?;
                // parse_definition can eat the trailing newline
                if *self.previous_token() == Token::Newline {
                    self.token_index -= 1;
                }

                let name = self.current_context.names[definition.name.item_name()].clone();
                id = self.new_top_level_id(name);
                TopLevelItemKind::Definition(definition)
            }
            Token::Type => {
                let definition = self.parse_type_definition()?;
                id = self.new_top_level_id(&definition.name);
                TopLevelItemKind::TypeDefinition(definition)
            }
            Token::Octothorpe => {
                let comptime = self.parse_comptime()?;
                // Hashing the whole comptime object here contains ExprIds which means this
                // top level id will not be stable if any of its contents change
                id = self.new_top_level_id(&comptime);
                TopLevelItemKind::Comptime(comptime)
            }
            _ => return self.expected("a top-level item"),
        };

        Ok(TopLevelItem { id, comments, kind })
    }

    fn parse_comments(&mut self) -> Vec<String> {
        let mut comments = Vec::new();

        while let Token::LineComment(comment) = self.current_token() {
            comments.push(comment.clone());
            self.advance();
            self.expect_newline_with_recovery("a newline after the comment");
        }

        comments
    }

    fn parse_definition(&mut self) -> Result<Definition> {
        let mutable = self.accept(Token::Mut);

        let start_location = self.current_token_location();
        let name = self.parse_definition_name()?;
        let parameters = self.parse_function_parameters();

        // If this is a lambda, reserve the lambda's expr id ahead of time so it is allocated
        // before the body for better cache performance
        let lambda = (parameters.len() != 0).then(|| self.reserve_expr());

        let mut typ = None;
        if self.accept(Token::Colon) {
            typ = self.parse_with_recovery(Self::parse_type, Token::Equal, &[Token::Newline, Token::Indent]).ok();
        }
        self.expect(Token::Equal, "`=` to begin the function body")?;

        let rhs = self.try_parse_or_recover_to_newline(|this| this.parse_block_or_expression()).unwrap_or_else(|| {
            self.push_expr(Expr::Error, self.current_token_location())
        });

        if let Some(id) = lambda {
            let lambda = Expr::Lambda(Lambda {
                parameters,
                return_type: typ,
                body: rhs,
            });
            self.insert_expr(id, lambda, start_location);
            Ok(Definition { mutable, name, typ: None, rhs: id })
        } else {
            Ok(Definition { mutable, name, typ, rhs })
        }
    }

    fn parse_definition_name(&mut self) -> Result<DefinitionName> {
        match self.current_token() {
            Token::TypeName(_) => {
                let type_name = self.parse_type_name_id()?;
                self.expect(Token::MemberAccess, "a `.` to separate this method's object type from its name")?;
                let item_name = self.parse_ident_id()?;
                Ok(DefinitionName::Method { type_name, item_name })
            }
            Token::Identifier(_) => {
                let name = self.parse_ident_id()?;
                Ok(DefinitionName::Single(name))
            }
            _ => self.expected("a definition name"),
        }
    }

    fn parse_type_definition(&mut self) -> Result<TypeDefinition> {
        self.expect(Token::Type, "`type`")?;
        let name = self.parse_type_name_id()?;
        let generics = self.many0(|this| this.parse_ident_id());
        self.expect(Token::Equal, "`=` to begin the type definition")?;
        let body = self.parse_type_body()?;
        Ok(TypeDefinition { name, generics, body })
    }

    fn parse_type_body(&mut self) -> Result<TypeDefinitionBody> {
        match self.current_token() {
            Token::Indent => self.parse_indented(Self::parse_indented_type_body),
            _ => self.parse_non_indented_type_body(),
        }
    }

    fn parse_indented_type_body(&mut self) -> Result<TypeDefinitionBody> {
        match self.current_token() {
            // struct
            Token::Identifier(_) => {
                let fields = self.delimited(|this| {
                    let field_name = this.parse_ident()?;
                    this.expect(Token::Colon, "a colon separating the field name from its type")?;
                    let field_type = this.parse_type()?;
                    this.accept(Token::Comma);
                    Ok((field_name, field_type))
                }, Token::Newline, false);
                Ok(TypeDefinitionBody::Struct(fields))
            }
            // enum
            Token::Pipe => {
                let variants = self.delimited(|this| {
                    this.expect(Token::Pipe, "`|`")?;
                    let variant_name = this.parse_type_name()?;
                    let parameters = this.many0(Self::parse_type); // TODO: arg type
                    Ok((variant_name, parameters))
                }, Token::Newline, false);
                Ok(TypeDefinitionBody::Enum(variants))
            }
            _ => self.expected("a field name or `|` to start this type body"),
        }
    }

    fn parse_non_indented_type_body(&mut self) -> Result<TypeDefinitionBody> {
        match self.current_token() {
            // struct
            Token::Identifier(_) => {
                let fields = self.delimited(|this| {
                    let field_name = this.parse_ident()?;
                    this.expect(Token::Colon, "a colon separating the field name from its type")?;
                    let field_type = this.parse_type()?;
                    Ok((field_name, field_type))
                }, Token::Comma, true);
                Ok(TypeDefinitionBody::Struct(fields))
            }
            // enum
            Token::Pipe => {
                let variants = self.many0(|this| {
                    this.expect(Token::Pipe, "`|`")?;
                    let variant_name = this.parse_type_name()?;
                    let parameters = this.many0(Self::parse_type); // TODO: arg type
                    Ok((variant_name, parameters))
                });
                Ok(TypeDefinitionBody::Enum(variants))
            }
            _ => self.expected("a field name or `|` to start this type body"),
        }
    }

    /// Parse an indented block using the given failable parser.
    /// On failure recovers to the unindent token and returns T::error_default.
    /// This only fails if there was no indent to begin with.
    fn parse_indented<T>(&mut self, parser: impl FnOnce(&mut Self) -> Result<T>) -> Result<T>
        where T: ErrorDefault
    {
        self.expect(Token::Indent, "an indent")?;

        let result = parser(self);
        if result.is_err() {
            self.recover_to(Token::Unindent, &[]);
        }

        if let Err(error) = self.expect(Token::Unindent, "an unindent") {
            // If we stopped short of the unindent, skip everything until the unindent
            self.diagnostics.push(error);
            self.recover_to(Token::Unindent, &[]);
            self.advance();
        }

        Ok(result.unwrap_or(T::error_default()))
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
            Token::TypeName(_) => {
                let path = self.parse_type_path_id()?;
                Ok(Type::Named(path))
            }
            Token::Identifier(_) => {
                let name = self.parse_ident_id()?;
                Ok(Type::Variable(name))
            }
            _ => self.expected("a type"),
        }
    }

    fn parse_type_name(&mut self) -> Result<String> {
        match self.current_token() {
            Token::TypeName(name) => {
                self.advance();
                Ok(name.clone())
            }
            _ => self.expected("a capitalized type name"),
        }
    }

    fn parse_ident(&mut self) -> Result<String> {
        match self.current_token() {
            Token::Identifier(name) => {
                self.advance();
                Ok(name.clone())
            }
            _ => self.expected("an identifier"),
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

    fn parse_function_parameters(&mut self) -> Vec<PatternId> {
        self.many0(Self::parse_function_parameter_pattern)
    }

    fn parse_function_parameter_pattern(&mut self) -> Result<PatternId> {
        self.with_pattern_id_and_location(Self::parse_function_parameter_pattern_inner)
    }

    fn parse_function_parameter_pattern_inner(&mut self) -> Result<Pattern> {
        match self.current_token() {
            Token::UnitLiteral => {
                self.advance();
                Ok(Pattern::Literal(Literal::Unit))
            }
            Token::ParenthesisLeft => {
                self.advance();
                let pattern = self.parse_with_recovery(Self::parse_pattern_inner, Token::ParenthesisRight, &[Token::Newline, Token::Equal])?;
                self.expect(Token::ParenthesisRight, "a `)` to close the opening `(` from the parameter")?;
                Ok(pattern)
            }
            Token::Identifier(_) => {
                let name = self.parse_ident_id()?;
                Ok(Pattern::Variable(name))
            }
            _ => self.expected("a parameter"),
        }
    }

    fn parse_pattern_inner(&mut self) -> Result<Pattern> {
        let start = self.current_token_span();
        let pattern = self.parse_function_parameter_pattern_inner()?;
        let end = self.previous_token_span();

        if self.accept(Token::Colon) {
            let location = start.to(&end).in_file(self.file_id);
            let pattern = self.push_pattern(pattern, location);
            let typ = self.parse_with_recovery(Self::parse_type, Token::ParenthesisRight, &[Token::Newline, Token::Equal])?;
            Ok(Pattern::TypeAnnotation(pattern, typ))
        } else {
            Ok(pattern)
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
            Token::Is => Some((6, false)),
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
        let function_location = span.in_file(self.file_id);

        let components = vec![(operator.to_string(), function_location.clone())]; // TODO: Variable::operator
        let path_id = self.push_path(Path { components }, function_location.clone());
        self.insert_expr(function, Expr::Variable(path_id), function_location);

        let call_expr = Expr::Call(Call { function, arguments: vec![lhs, rhs] });
        self.insert_expr(call, call_expr, location);
        results.push(call);
    }

    fn parse_expression(&mut self) -> Result<ExprId> {
        match self.current_token() {
            Token::If => self.parse_if_expr(),
            Token::Match => self.parse_match(),
            _ => self.parse_shunting_yard(),
        }
    }

    /// Parse an arbitrary infix expression using the shunting-yard algorithm
    fn parse_shunting_yard(&mut self) -> Result<ExprId> {
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
            self.advance();

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
                self.parse_function_call_or_atom()
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

                let operator_location = self.current_token_location();
                self.advance();
                let rhs = self.parse_unary()?;
                let location = operator_location.to(&self.expr_location(rhs));

                let components = vec![(operator.to_string(), operator_location.clone())];
                let path_id = self.push_path(Path { components }, operator_location.clone());
                self.insert_expr(function_id, Expr::Variable(path_id), operator_location);

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
                    let components = vec![(Token::At.to_string(), operator_location.clone())];
                    let path_id = this.push_path(Path { components }, operator_location.clone());
                    let function = this.push_expr(Expr::Variable(path_id), operator_location);
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
                        this.expect(Token::BracketRight, "a `]` to terminate the index expression")?;
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
            _ => self.expected("an expression"),
        }
    }

    fn parse_sequence_item(&mut self) -> Result<SequenceItem> {
        let comments = self.parse_comments();
        let expr = self.parse_statement()?;
        Ok(SequenceItem { comments, expr })
    }

    fn parse_statement(&mut self) -> Result<ExprId> {
        let start = self.current_token_span();
        let previous_position = self.token_index;

        if let Ok(definition) = self.parse_definition() {
            let end = self.previous_token_span();
            let location = start.to(&end).in_file(self.file_id);
            let expr = Expr::Definition(definition);
            return Ok(self.push_expr(expr, location));
        }

        self.token_index = previous_position;
        self.parse_expression()
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

    fn with_pattern_id_and_location(&mut self, f: impl FnOnce(&mut Self) -> Result<Pattern>) -> Result<PatternId> {
        let id = self.reserve_pattern();
        let (pattern, location) = self.with_location(f)?;
        self.insert_pattern(id, pattern, location);
        Ok(id)
    }

    fn parse_if(&mut self, mut body: impl Copy + FnMut(&mut Self) -> Result<ExprId>) -> Result<ExprId> {
        self.with_expr_id_and_location(|this| {
            this.expect(Token::If, "a `if` to begin an if expression")?;

            let condition = this.parse_expr_with_recovery(Self::parse_block_or_expression, Token::Then, &[Token::Newline])?;

            this.expect(Token::Then, "a `then` to end this if condition")?;

            let then = this.parse_expr_with_recovery(body, Token::Else, &[Token::Newline])?;

            let else_ = if this.accept(Token::Else) {
                Some(body(this)?)
            } else {
                None
            };
            Ok(Expr::If(cst::If { condition, then, else_ }))
        })
    }

    fn parse_if_expr(&mut self) -> Result<ExprId> {
        self.parse_if(Self::parse_block_or_expression)
    }

    /// A comptime if, unlike a regular if, requires a block so that we can quote
    /// every token until we find the matching unindent.
    fn parse_comptime_if(&mut self) -> Result<ExprId> {
        self.parse_if(Self::parse_quoted_block)
    }

    fn parse_match(&mut self) -> Result<ExprId> {
        self.with_expr_id_and_location(|this| {
            this.expect(Token::Match, "`match` to start this match expression")?;

            let expression = this.parse_expression()?;

            let cases = this.many0(|this| {
                this.expect(Token::Pipe, "a `|` to start a new pattern")?;
                let pattern = this.parse_function_parameter_pattern()?;
                this.expect(Token::RightArrow, "a `->` to separate the match pattern from the match branch")?;
                let branch = this.parse_block_or_expression()?;
                Ok((pattern, branch))
            });

            Ok(Expr::Match(cst::Match { expression, cases }))
        })
    }

    /// Parse an indent followed by any arbitrary tokens until a matching unindent
    fn parse_quoted_block(&mut self) -> Result<ExprId> {
        self.expect(Token::Indent, "an indent to start a quoted block")?;
        let mut indent_count = 0;
        let mut tokens = Vec::new();

        self.with_expr_id_and_location(|this| {
            loop {
                this.advance();
                match this.current_token() {
                    Token::Indent => {
                        indent_count += 1;
                        tokens.push(Token::Indent);
                    }
                    Token::Unindent => {
                        if indent_count == 0 {
                            break;
                        }
                        indent_count -= 1;
                    }
                    // This should be unreachable since the lexer should guarantee indents are
                    // always matched.
                    Token::EndOfInput => break,
                    other => tokens.push(other.clone()),
                }
            }
            Ok(Expr::Quoted(cst::Quoted { tokens }))
        })
    }

    fn parse_block_or_expression(&mut self) -> Result<ExprId> {
        match self.current_token() {
            Token::Indent => self.parse_block(),
            _ => self.parse_expression(),
        }
    }

    fn parse_block(&mut self) -> Result<ExprId> {
        let (expr, location) = self.with_location(|this| {
            this.parse_indented(|this| {
                let statements = this.delimited(Self::parse_sequence_item, Token::Newline, true);
                Ok(Expr::Sequence(statements))
            })
        })?;
        Ok(self.push_expr(expr, location))
    }

    /// Create a location from the current token before running the given parse function to the
    /// current token (end exclusive) after running the given parse function.
    fn with_location<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<(T, Location)> {
        let start = self.current_token_span();
        let ret = f(self)?;
        let end = self.previous_token_span();
        Ok((ret, start.to(&end).in_file(self.file_id)))
    }

    fn parse_variable(&mut self) -> Result<ExprId> {
        let path = self.parse_value_path_id()?;
        let location = self.current_context.path_locations[path].clone();
        Ok(self.push_expr(Expr::Variable(path), location))
    }

    fn parse_function_call_or_atom(&mut self) -> Result<ExprId> {
        let function = self.parse_atom()?;

        if let Ok(arguments) = self.many1(Self::parse_function_arg) {
            let last_arg_location = self.expr_location(*arguments.last().unwrap());
            let location = self.expr_location(function).to(&last_arg_location);
            let call = Expr::Call(Call { function, arguments });
            Ok(self.push_expr(call, location))
        } else {
            Ok(function)
        }
    }

    fn parse_string(&mut self, contents: String) -> Result<ExprId> {
        let location = self.current_token_location();
        self.advance();
        Ok(self.push_expr(Expr::Literal(Literal::String(contents)), location))
    }

    fn parse_comptime(&mut self) -> Result<Comptime> {
        // Skip `#`
        self.advance();

        match self.current_token() {
            Token::If => {
                let if_ = self.parse_comptime_if()?;
                Ok(Comptime::Expr(if_))
            }
            Token::Identifier(_) | Token::TypeName(_) => {
                let call = self.parse_expr_with_recovery(Self::parse_function_call_or_atom, Token::Newline, &[])?;
                Ok(Comptime::Expr(call))
            }
            _ => self.expected("a compile-time item"),
        }
    }

    fn parse_value_path_id(&mut self) -> Result<PathId> {
        let (path, location) = self.with_location(Self::parse_value_path)?;
        Ok(self.push_path(path, location))
    }

    fn parse_type_path_id(&mut self) -> Result<PathId> {
        let (path, location) = self.with_location(Self::parse_type_path)?;
        Ok(self.push_path(path, location))
    }

    fn parse_ident_id(&mut self) -> Result<NameId> {
        let name = Arc::new(self.parse_ident()?);
        let location = self.previous_token_location();
        Ok(self.push_name(name, location))
    }

    fn parse_type_name_id(&mut self) -> Result<NameId> {
        let name = Arc::new(self.parse_type_name()?);
        let location = self.previous_token_location();
        Ok(self.push_name(name, location))
    }
}
