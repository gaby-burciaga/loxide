use loxide_parse::{ParseSess, Parser, SymbolGallery};

fn main() {
    let src = r#"(1+3*3)/(5+1)"#;

    let psess = ParseSess {
        symbol_gallery: SymbolGallery::default(),
    };

    let stream = loxide_parse::lexer::lex_token_trees(&psess, src);
    let mut parser = Parser::new(&psess, stream);
    let expr = parser.parse_expr();
    println!("{expr:#?}");
}
