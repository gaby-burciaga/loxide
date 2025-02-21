use loxide_span::Span;

use crate::token::{Delimiter, Token};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenStream(Vec<TokenTree>);

impl TokenStream {
    pub fn new(tts: Vec<TokenTree>) -> Self {
        Self(tts)
    }

    pub fn get(&self, index: usize) -> Option<&TokenTree> {
        self.0.get(index)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenTree {
    Token(Token),
    Delimited(DelimSpan, Delimiter, TokenStream),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DelimSpan {
    pub open: Span,
    pub close: Span,
}

impl DelimSpan {
    pub fn from_pair(open: Span, close: Span) -> Self {
        DelimSpan { open, close }
    }
}
