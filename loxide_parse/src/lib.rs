#![allow(dead_code)]

use std::cell::RefCell;

use indexmap::IndexMap;
use loxide_ast::{
    BinOp, BinOpKind, Expr, ExprKind, UnOp,
    token::{BinOpToken, Delimiter, Lit, Token, TokenKind},
    tokenstream::{TokenStream, TokenTree},
};
use loxide_span::{DUMMY_SP, Span, symbol::Symbol};

pub mod lexer;

pub struct Parser<'a> {
    psess: &'a ParseSess,
    token: Token,
    prev_token: Token,
    token_cursor: TokenCursor,
}

impl<'a> Parser<'a> {
    pub fn new(psess: &'a ParseSess, stream: TokenStream) -> Self {
        let mut parser = Self {
            psess,
            token: Token::dummy(),
            prev_token: Token::dummy(),
            token_cursor: TokenCursor {
                curr: TokenTreeCursor::new(stream),
                stack: Vec::new(),
            },
        };

        parser.bump();

        parser
    }

    // FIXME: Remove pub
    pub fn parse_expr(&mut self) -> Box<Expr> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Box<Expr> {
        self.parse_expr_common(&[TokenKind::EqEq, TokenKind::Ne], Self::parse_comparison)
    }

    fn parse_comparison(&mut self) -> Box<Expr> {
        self.parse_expr_common(
            &[TokenKind::Lt, TokenKind::Le, TokenKind::Gt, TokenKind::Ge],
            Self::parse_term,
        )
    }

    fn parse_term(&mut self) -> Box<Expr> {
        self.parse_expr_common(
            &[
                TokenKind::BinOp(BinOpToken::Plus),
                TokenKind::BinOp(BinOpToken::Minus),
            ],
            Parser::parse_factor,
        )
    }

    fn parse_factor(&mut self) -> Box<Expr> {
        self.parse_expr_common(
            &[
                TokenKind::BinOp(BinOpToken::Star),
                TokenKind::BinOp(BinOpToken::Slash),
            ],
            Parser::parse_unary,
        )
    }

    fn parse_expr_common<P>(&mut self, tokens: &[TokenKind], mut parser: P) -> Box<Expr>
    where
        P: FnMut(&mut Self) -> Box<Expr>,
    {
        let expr = parser(self);

        while self.is_token_ahead(0, tokens) {
            let op = BinOp {
                node: BinOpKind::from_token(&self.token).expect("Token should be BinOp"),
                span: self.token.span,
            };

            self.bump();

            let right = parser(self);
            let span = expr.span.to(right.span);

            return self.mk_expr(self.mk_binary(op, expr, right), span);
        }

        expr
    }

    fn parse_unary(&mut self) -> Box<Expr> {
        if self.is_token_ahead(0, &[TokenKind::BinOp(BinOpToken::Minus), TokenKind::Not]) {
            let lo = self.token.span;
            // FIXME: Op
            let op = UnOp::Not;
            self.bump();

            let right = self.parse_unary();
            let span = lo.to(right.span);
            self.mk_expr(self.mk_unary(op, right), span)
        } else {
            self.parse_opt_expr_lit()
                .unwrap_or_else(|| match self.token.kind {
                    TokenKind::OpenDelim(Delimiter::Paren) => self.parse_expr_in_paren(),
                    _ => panic!("Expected exper"),
                })
        }
    }

    fn parse_opt_expr_lit(&mut self) -> Option<Box<Expr>> {
        let lo = self.token.span;

        let (lit, _) = self.parse_opt_token_lit()?;
        let expr = self.mk_expr(ExprKind::Lit(lit), lo.to(self.prev_token.span));
        Some(expr)
    }

    fn parse_expr_in_paren(&mut self) -> Box<Expr> {
        let lo = self.token.span;
        self.bump();
        let mut expr = self.parse_expr();

        if self.token.kind != TokenKind::CloseDelim(Delimiter::Paren) {
            panic!("Expected closing ')'");
        }

        let hi = self.token.span;

        self.bump();

        expr.span = lo.to(hi);

        expr
    }

    fn parse_opt_token_lit(&mut self) -> Option<(Lit, Span)> {
        let span = self.token.span;

        Lit::from_token(&self.token).map(|lit| {
            self.bump();
            (lit, span)
        })
    }

    fn mk_expr(&self, kind: ExprKind, span: Span) -> Box<Expr> {
        Box::new(Expr { kind, span })
    }

    fn mk_binary(&self, op: BinOp, lhs: Box<Expr>, rhs: Box<Expr>) -> ExprKind {
        ExprKind::Binary(op, lhs, rhs)
    }

    fn mk_unary(&self, op: UnOp, expr: Box<Expr>) -> ExprKind {
        ExprKind::Unary(op, expr)
    }

    fn bump(&mut self) {
        self.prev_token = std::mem::replace(&mut self.token, self.token_cursor.next());
    }

    fn is_token_ahead(&self, dist: usize, tokens: &[TokenKind]) -> bool {
        self.look_ahead(dist, |t| tokens.iter().any(|&kind| t.kind == kind))
    }

    fn look_ahead<R>(&self, dist: usize, looker: impl FnOnce(&Token) -> R) -> R {
        if dist == 0 {
            return looker(&self.token);
        }

        // TODO: Fast Special case for diset == 1

        let mut cursor = self.token_cursor.clone();
        let mut i = 0;

        while i < dist.saturating_sub(1) {
            cursor.next();
            i += 1;
        }

        looker(&cursor.next())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TokenCursor {
    curr: TokenTreeCursor,
    stack: Vec<TokenTreeCursor>,
}

impl TokenCursor {
    fn next(&mut self) -> Token {
        if let Some(tree) = self.curr.curr() {
            match tree {
                &TokenTree::Token(token) => {
                    self.curr.bump();
                    return token;
                }
                &TokenTree::Delimited(sp, delim, ref tts) => {
                    let trees = TokenTreeCursor::new(tts.clone());
                    self.stack.push(std::mem::replace(&mut self.curr, trees));
                    return Token::new(TokenKind::OpenDelim(delim), sp.open);
                }
            }
        }

        if let Some(parent) = self.stack.pop() {
            let Some(&TokenTree::Delimited(sp, delim, _)) = parent.curr() else {
                panic!("Parent should be Delimited")
            };

            self.curr = parent;
            self.curr.bump();

            return Token::new(TokenKind::CloseDelim(delim), sp.close);
        }

        return Token::new(TokenKind::Eof, DUMMY_SP);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TokenTreeCursor {
    stream: TokenStream,
    index: usize,
}

impl TokenTreeCursor {
    #[inline]
    fn new(stream: TokenStream) -> Self {
        Self { stream, index: 0 }
    }

    #[inline]
    fn curr(&self) -> Option<&TokenTree> {
        self.stream.get(self.index)
    }

    #[inline]
    fn bump(&mut self) {
        self.index += 1;
    }
}

pub struct ParseSess {
    pub symbol_gallery: SymbolGallery,
}

#[derive(Default)]
pub struct SymbolGallery {
    pub symbols: RefCell<IndexMap<Symbol, Span>>,
}

impl SymbolGallery {
    pub fn insert(&self, symbol: Symbol, span: Span) {
        self.symbols.borrow_mut().entry(symbol).or_insert(span);
    }
}
