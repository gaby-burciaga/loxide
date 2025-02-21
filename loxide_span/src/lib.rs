use symbol::Interner;

pub mod symbol;

pub const DUMMY_SP: Span = Span { lo: 0, hi: 0 };

thread_local! {
    pub(crate) static SESSION_GLOBALS: SessionGlobals = SessionGlobals::new();
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    lo: u32,
    hi: u32,
}

impl Span {
    pub fn new(mut lo: u32, mut hi: u32) -> Self {
        if lo > hi {
            std::mem::swap(&mut lo, &mut hi);
        }

        Span { lo, hi }
    }

    pub fn len(&self) -> u32 {
        self.hi - self.lo
    }

    pub fn to(self, end: Span) -> Span {
        Span::new(self.lo.min(end.lo), self.hi.max(end.hi))
    }
}

pub fn with_session_globals<F, R>(f: F) -> R
where
    F: FnOnce(&SessionGlobals) -> R,
{
    SESSION_GLOBALS.with(f)
}

pub struct SessionGlobals {
    symbol_interner: Interner,
}

impl SessionGlobals {
    pub fn new() -> Self {
        let symbol_interner = Interner::prefill(&[
            "fn", "struct", "loop", "while", "if", "else", "let", "mut", "return", "true", "false",
        ]);
        SessionGlobals { symbol_interner }
    }
}
