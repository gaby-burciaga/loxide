#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u32,
}

impl Token {
    pub fn new(kind: TokenKind, len: u32) -> Self {
        Self { kind, len }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    Whitespace,

    Ident,

    Literal(LiteralKind),

    Dot,
    Comma,
    Semicolon,

    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,

    Eq,
    Bang,
    Lt,
    Gt,

    Or,
    And,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    Unknown,

    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LiteralKind {
    Int,
    Float,
    Char { is_terminated: bool },
    String { is_terminated: bool },
}
