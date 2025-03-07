use core::fmt;

use loxide_span::{DUMMY_SP, Span, symbol::Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn dummy() -> Self {
        Token::new(TokenKind::Eof, DUMMY_SP)
    }

    pub fn glue(&self, joint: &Token) -> Option<Self> {
        let kind = match self.kind {
            TokenKind::Eq => match joint.kind {
                TokenKind::Eq => TokenKind::EqEq,
                _ => return None,
            },
            TokenKind::Not => match joint.kind {
                TokenKind::Eq => TokenKind::Ne,
                _ => return None,
            },
            TokenKind::Lt => match joint.kind {
                TokenKind::Eq => TokenKind::Le,
                TokenKind::Lt => TokenKind::BinOp(BinOpToken::Shl),
                TokenKind::Le => TokenKind::BinOpEq(BinOpToken::Shl),
                _ => return None,
            },
            TokenKind::Gt => match joint.kind {
                TokenKind::Eq => TokenKind::Ge,
                TokenKind::Gt => TokenKind::BinOp(BinOpToken::Shr),
                TokenKind::Ge => TokenKind::BinOpEq(BinOpToken::Shr),
                _ => return None,
            },
            TokenKind::BinOp(op) => match joint.kind {
                TokenKind::Eq => TokenKind::BinOpEq(op),
                TokenKind::BinOp(BinOpToken::Or) if op == BinOpToken::Or => TokenKind::OrOr,
                TokenKind::BinOp(BinOpToken::And) if op == BinOpToken::And => TokenKind::AndAnd,
                _ => return None,
            },

            _ => return None,
        };

        Some(Token::new(kind, self.span.to(joint.span)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    Ident(Symbol),

    Literal(Lit),

    Dot,
    Comma,
    Semicolon,

    OpenDelim(Delimiter),
    CloseDelim(Delimiter),

    Eq,
    EqEq,
    Not,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    OrOr,
    AndAnd,

    BinOp(BinOpToken),
    BinOpEq(BinOpToken),

    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lit {
    pub kind: LitKind,
    pub symbol: Symbol,
}

impl Lit {
    pub fn new(kind: LitKind, symbol: Symbol) -> Self {
        Self { kind, symbol }
    }

    pub fn from_token(token: &Token) -> Option<Lit> {
        match token.kind {
            TokenKind::Ident(name) if name.is_bool_lit() => Some(Lit::new(LitKind::Bool, name)),
            TokenKind::Literal(lit) => Some(lit),
            _ => None,
        }
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Lit { kind, symbol } = *self;
        match kind {
            LitKind::Char => write!(f, "'{symbol}'"),
            LitKind::String => write!(f, "\"{symbol}\""),
            _ => write!(f, "{symbol}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LitKind {
    Bool,
    Int,
    Float,
    Char,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOpToken {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Or,
    And,
    Shl,
    Shr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Delimiter {
    Paren,
    Brace,
    Bracket,
}
