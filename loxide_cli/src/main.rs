use loxide_parse::{ParseSess, Parser, SymbolGallery};

fn main() {
    let src = r#"fn main() {
        let msg = "Hello world";
        let n = 10;
    }"#;

    let psess = ParseSess {
        symbol_gallery: SymbolGallery::default(),
    };

    let stream = loxide_parse::lexer::lex_token_trees(&psess, src);
    let mut parser = Parser::new(&psess, stream);
    let block = parser.parse_item();
    println!("{block:#?}");
}
