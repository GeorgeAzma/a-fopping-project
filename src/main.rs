mod debug;
mod lexer;
mod parser;
pub(crate) use lexer::*;
pub(crate) use parser::*;

fn main() {
    // ignore, this is for better error messages
    debug::set_panic_hook();

    let code = include_str!("test.mar");
    let ast = Parser::new(code);

    println!("{ast:?}");
}
