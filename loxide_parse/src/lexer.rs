use loxide_ast::{
    token::{BinOpToken, Delimiter, Lit, LitKind, Token, TokenKind},
    tokenstream::{DelimSpan, TokenStream, TokenTree},
};
use loxide_lexer::Cursor;
use loxide_span::{Span, symbol::Symbol};

use crate::ParseSess;

pub fn lex_token_trees(psess: &ParseSess, src: &str) -> TokenStream {
    let cursor = Cursor::new(src);

    let mut lexer = Lexer {
        psess,
        pos: 0,
        src,
        cursor,
        token: Token::dummy(),
    };

    let stream = lexer.lex_token_trees(false);

    stream
}

struct Lexer<'psess, 'src> {
    psess: &'psess ParseSess,
    pos: u32,
    src: &'src str,
    cursor: Cursor<'src>,
    token: Token,
}

impl<'psess, 'src> Lexer<'psess, 'src> {
    fn lex_token_trees(&mut self, is_delimited: bool) -> TokenStream {
        // Skip opening delimiter, or dummy token.
        self.bump();

        let mut buf = Vec::new();

        loop {
            match self.token.kind {
                TokenKind::OpenDelim(delim) => buf.push(self.lex_token_tree_open_delim(delim)),
                TokenKind::CloseDelim(_) => {
                    if !is_delimited {
                        panic!("Not matching closing delimiter");
                    }

                    return TokenStream::new(buf);
                }
                TokenKind::Eof => {
                    if is_delimited {
                        panic!("Unclosed delimiter to EOF")
                    }

                    return TokenStream::new(buf);
                }
                _ => {
                    let this_tok = self.bump();
                    buf.push(TokenTree::Token(this_tok));
                }
            }
        }
    }

    fn lex_token_tree_open_delim(&mut self, open_delim: Delimiter) -> TokenTree {
        let pre_span = self.token.span;

        let tts = self.lex_token_trees(true);

        let delim_span = DelimSpan::from_pair(pre_span, self.token.span);

        match self.token.kind {
            TokenKind::CloseDelim(close_delim) if close_delim == open_delim => {
                self.bump();
            }
            TokenKind::CloseDelim(_) => {
                panic!("No matching closing delimiter");
            }
            TokenKind::Eof => {}
            _ => unreachable!(),
        }

        TokenTree::Delimited(delim_span, open_delim, tts)
    }

    fn bump(&mut self) -> Token {
        let next_tok = loop {
            let (next_tok, preceded_by_whitespace) = self.next_token_from_cursor();

            if preceded_by_whitespace {
                break next_tok;
            } else if let Some(glued) = self.token.glue(&next_tok) {
                self.token = glued;
            } else {
                break next_tok;
            }
        };

        std::mem::replace(&mut self.token, next_tok)
    }

    fn next_token_from_cursor(&mut self) -> (Token, bool) {
        let mut preceded_by_whitespace = false;

        loop {
            let token = self.cursor.next_token();
            let start = self.pos;
            self.pos += token.len;

            let kind = match token.kind {
                loxide_lexer::TokenKind::Whitespace => {
                    preceded_by_whitespace = true;
                    continue;
                }

                loxide_lexer::TokenKind::Ident => self.ident(start),

                loxide_lexer::TokenKind::Literal(kind) => {
                    let (kind, symbol) = self.cook_lexer_literal(kind, start);
                    TokenKind::Literal(Lit { kind, symbol })
                }

                loxide_lexer::TokenKind::Dot => TokenKind::Dot,
                loxide_lexer::TokenKind::Comma => TokenKind::Comma,
                loxide_lexer::TokenKind::Semicolon => TokenKind::Semicolon,

                loxide_lexer::TokenKind::OpenParen => TokenKind::OpenDelim(Delimiter::Paren),
                loxide_lexer::TokenKind::CloseParen => TokenKind::CloseDelim(Delimiter::Paren),
                loxide_lexer::TokenKind::OpenBrace => TokenKind::OpenDelim(Delimiter::Brace),
                loxide_lexer::TokenKind::CloseBrace => TokenKind::CloseDelim(Delimiter::Brace),
                loxide_lexer::TokenKind::OpenBracket => TokenKind::OpenDelim(Delimiter::Bracket),
                loxide_lexer::TokenKind::CloseBracket => TokenKind::CloseDelim(Delimiter::Bracket),

                loxide_lexer::TokenKind::Eq => TokenKind::Eq,
                loxide_lexer::TokenKind::Bang => TokenKind::Not,
                loxide_lexer::TokenKind::Lt => TokenKind::Lt,
                loxide_lexer::TokenKind::Gt => TokenKind::Gt,

                loxide_lexer::TokenKind::Or => TokenKind::BinOp(BinOpToken::Or),
                loxide_lexer::TokenKind::And => TokenKind::BinOp(BinOpToken::And),
                loxide_lexer::TokenKind::Minus => TokenKind::BinOp(BinOpToken::Slash),
                loxide_lexer::TokenKind::Star => TokenKind::BinOp(BinOpToken::Slash),
                loxide_lexer::TokenKind::Slash => TokenKind::BinOp(BinOpToken::Slash),
                loxide_lexer::TokenKind::Percent => TokenKind::BinOp(BinOpToken::Percent),

                loxide_lexer::TokenKind::Unknown => todo!("Unknown character"),

                loxide_lexer::TokenKind::Eof => TokenKind::Eof,
                loxide_lexer::TokenKind::Plus => todo!(),
            };

            return (
                Token::new(kind, Span::new(start, self.pos)),
                preceded_by_whitespace,
            );
        }
    }

    fn ident(&mut self, start: u32) -> TokenKind {
        let sym = Symbol::intern(self.str_from(start));
        let span = Span::new(start, self.pos);

        self.psess.symbol_gallery.insert(sym, span);

        TokenKind::Ident(sym)
    }

    fn cook_lexer_literal(
        &mut self,
        kind: loxide_lexer::LiteralKind,
        start: u32,
    ) -> (LitKind, Symbol) {
        match kind {
            loxide_lexer::LiteralKind::Int => (LitKind::Int, self.symbol_from(start)),
            loxide_lexer::LiteralKind::Float => (LitKind::Float, self.symbol_from(start)),
            loxide_lexer::LiteralKind::Char { is_terminated } => {
                if !is_terminated {
                    todo!("Char not terminated");
                }

                (LitKind::Char, self.symbol_from_to(start + 1, self.pos - 1))
            }
            loxide_lexer::LiteralKind::String { is_terminated } => {
                if !is_terminated {
                    todo!("Char not terminated");
                }

                (
                    LitKind::String,
                    self.symbol_from_to(start + 1, self.pos - 1),
                )
            }
        }
    }

    fn symbol_from(&self, start: u32) -> Symbol {
        Symbol::intern(self.str_from(start))
    }

    fn symbol_from_to(&self, start: u32, end: u32) -> Symbol {
        Symbol::intern(self.str_from_to(start, end))
    }

    fn str_from(&self, start: u32) -> &str {
        self.str_from_to(start, self.pos)
    }

    fn str_from_to(&self, start: u32, end: u32) -> &str {
        &self.src[start as usize..end as usize]
    }
}
