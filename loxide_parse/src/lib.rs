use std::cell::RefCell;

use indexmap::IndexMap;
use loxide_span::{Span, symbol::Symbol};

pub mod lexer;

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
