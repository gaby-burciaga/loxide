use loxide_span::Span;
use token::{BinOpToken, Lit, Token, TokenKind};

pub mod token;
pub mod tokenstream;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Item {
    pub kind: ItemKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ItemKind {}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExprKind {
    Unary(UnOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Lit(Lit),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BinOp {
    pub node: BinOpKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Or,
    And,
    BitOr,
    BitAnd,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl BinOpKind {
    pub fn from_token(token: &Token) -> Option<Self> {
        match token.kind {
            TokenKind::EqEq => Some(BinOpKind::Eq),
            TokenKind::Ne => Some(BinOpKind::Ne),
            TokenKind::Lt => Some(BinOpKind::Lt),
            TokenKind::Le => Some(BinOpKind::Le),
            TokenKind::Gt => Some(BinOpKind::Gt),
            TokenKind::Ge => Some(BinOpKind::Ge),
            TokenKind::OrOr => Some(BinOpKind::Or),
            TokenKind::AndAnd => Some(BinOpKind::And),
            TokenKind::BinOp(bin_op_token) => Some(match bin_op_token {
                BinOpToken::Plus => BinOpKind::Add,
                BinOpToken::Minus => BinOpKind::Sub,
                BinOpToken::Star => BinOpKind::Mul,
                BinOpToken::Slash => BinOpKind::Div,
                BinOpToken::Percent => BinOpKind::Rem,
                BinOpToken::Or => BinOpKind::BitOr,
                BinOpToken::And => BinOpKind::BitAnd,
                BinOpToken::Shl => BinOpKind::Shl,
                BinOpToken::Shr => BinOpKind::Shr,
            }),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnOp {
    Not,
    Neg,
}
