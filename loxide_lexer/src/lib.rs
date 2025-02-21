use std::str::Chars;

pub mod token;

pub use token::*;

pub fn lex(src: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(src);

    std::iter::from_fn(move || {
        let token = cursor.next_token();

        match token.kind {
            TokenKind::Eof => None,
            _ => Some(token),
        }
    })
}

const EOF_CHAR: char = '\0';

pub struct Cursor<'src> {
    chars: Chars<'src>,
    len_remaining: usize,
}

impl<'src> Cursor<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            chars: src.chars(),
            len_remaining: src.len(),
        }
    }

    pub fn next_token(&mut self) -> Token {
        let kind = self.lex_token_kind();
        let len = self.token_len();

        Token::new(kind, len)
    }

    fn lex_token_kind(&mut self) -> TokenKind {
        match self.bump() {
            Some(c) => match c {
                c if c.is_whitespace() => self.lex_whitespace(),

                c if is_id_start(c) => self.lex_ident(),

                '\'' => self.lex_char(),
                '"' => self.lex_string(),

                '0'..='9' => self.lex_number(),

                '.' => TokenKind::Dot,
                ',' => TokenKind::Comma,
                ';' => TokenKind::Semicolon,

                '(' => TokenKind::OpenParen,
                ')' => TokenKind::CloseParen,
                '{' => TokenKind::OpenBrace,
                '}' => TokenKind::CloseBrace,
                '[' => TokenKind::OpenBracket,
                ']' => TokenKind::CloseBracket,

                '|' => TokenKind::Or,
                '&' => TokenKind::And,

                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '*' => TokenKind::Star,
                '/' => TokenKind::Slash,
                '%' => TokenKind::Percent,

                _ => TokenKind::Unknown,
            },
            None => TokenKind::Eof,
        }
    }

    fn lex_ident(&mut self) -> TokenKind {
        self.eat_while(is_id_continue);
        TokenKind::Ident
    }

    fn lex_string(&mut self) -> TokenKind {
        let is_terminated = self.lex_quoted('"');
        TokenKind::Literal(LiteralKind::String { is_terminated })
    }

    fn lex_char(&mut self) -> TokenKind {
        let is_terminated = self.lex_quoted('\'');
        TokenKind::Literal(LiteralKind::Char { is_terminated })
    }

    fn lex_quoted(&mut self, quote: char) -> bool {
        while let Some(c) = self.bump() {
            match c {
                c if c == quote => return true,
                '\\' if self.first() == '\\' || self.first() == quote => {
                    self.bump();
                }
                _ => {}
            }
        }

        false
    }

    fn lex_number(&mut self) -> TokenKind {
        let is_valid_number = |c: char| c == '_' || c.is_ascii_digit();
        self.eat_while(is_valid_number);

        if self.first() == '.' {
            self.bump();
            self.eat_while(is_valid_number);
            TokenKind::Literal(LiteralKind::Float)
        } else {
            TokenKind::Literal(LiteralKind::Int)
        }
    }

    fn lex_whitespace(&mut self) -> TokenKind {
        self.eat_while(char::is_whitespace);
        TokenKind::Whitespace
    }

    fn token_len(&mut self) -> u32 {
        let len = self.len_remaining - self.chars.as_str().len();
        self.len_remaining = self.chars.as_str().len();
        len as u32
    }

    fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn first(&mut self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    fn eat_while(&mut self, mut pred: impl FnMut(char) -> bool) {
        while !self.is_eof() && pred(self.first()) {
            self.bump();
        }
    }

    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }
}

fn is_id_start(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

fn is_id_continue(c: char) -> bool {
    c == '_' || c.is_alphanumeric()
}
