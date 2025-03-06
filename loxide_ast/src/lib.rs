use loxide_span::{Span, symbol::Symbol};
use token::{BinOpToken, Lit, Token, TokenKind};

pub mod token;
pub mod tokenstream;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Item {
    pub kind: ItemKind,
    pub ident: Ident,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ItemKind {
    Fn(Box<Fn>),
    Struct(Box<Struct>)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub ident: Ident,
    pub fields: Vec<Field>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    pub ident: Ident,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident {
    pub symbol: Symbol,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fn {
    pub sig: FnSig,
    pub body: Box<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnSig {
    pub inputs: Vec<Param>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Param {
    pub ident: Ident,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StmtKind {
    Let(Box<Local>),
    Expr(Box<Expr>),
    While,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local {
    pub ident: Ident,
    pub init: Box<Expr>,
    pub span: Span,
}

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
    Call(Box<Expr>, Vec<Box<Expr>>),
    Ident(Ident),
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

impl UnOp {
    pub fn from_token(token: &Token) -> Option<Self> {
        match token.kind {
            TokenKind::Not => Some(UnOp::Not),
            TokenKind::BinOp(BinOpToken::Minus) => Some(UnOp::Neg),
            _ => None,
        }
    }
}
