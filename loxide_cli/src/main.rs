use loxide_parse::{ParseSess, SymbolGallery};

fn main() {
    let src = r#"( fn )"#;

    let psess = ParseSess {
        symbol_gallery: SymbolGallery::default(),
    };

    let stream = loxide_parse::lexer::lex_token_trees(&psess, src);

    println!("{stream:#?}");
}
