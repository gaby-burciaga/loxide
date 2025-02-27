use core::fmt;
use std::cell::RefCell;

use indexmap::IndexSet;
use typed_arena::Arena;

use crate::with_session_globals;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(u32);

impl Symbol {
    const fn new(n: u32) -> Self {
        Self(n)
    }

    pub fn intern(string: &str) -> Self {
        with_session_globals(|session_globals| session_globals.symbol_interner.intern(string))
    }

    pub fn as_str(&self) -> &str {
        with_session_globals(|session_globals| unsafe {
            std::mem::transmute::<&str, &str>(session_globals.symbol_interner.get(*self))
        })
    }

    pub fn as_u32(&self) -> u32 {
        self.0
    }

    pub fn is_bool_lit(self) -> bool {
        // TODO: make keyword table
        // FIXME: for now a simple string equality check
        let str = self.as_str();
        str == "true" || str == "false"
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

pub(crate) struct Interner(RefCell<InternerInner>);

impl Interner {
    pub fn prefill(init: &[&'static str]) -> Self {
        Self(RefCell::new(InternerInner {
            arena: Arena::new(),
            strings: init.iter().copied().collect(),
        }))
    }

    pub fn intern(&self, string: &str) -> Symbol {
        let mut inner = self.0.borrow_mut();

        if let Some(idx) = inner.strings.get_index_of(string) {
            return Symbol::new(idx as u32);
        }

        let string: &str = inner.arena.alloc_str(string);
        let string: &'static str = unsafe { &*(string as *const str) };

        let (idx, _) = inner.strings.insert_full(string);

        Symbol::new(idx as u32)
    }

    pub fn get(&self, symbol: Symbol) -> &str {
        self.0
            .borrow()
            .strings
            .get_index(symbol.0 as usize)
            .unwrap()
    }
}

struct InternerInner {
    arena: Arena<u8>,
    strings: IndexSet<&'static str>,
}
