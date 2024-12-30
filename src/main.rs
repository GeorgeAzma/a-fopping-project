mod debug;
mod interpreter;
mod lexer;
mod parser;
use interpreter::Interpreter;
pub(crate) use lexer::*;
pub(crate) use parser::*;

fn main() {
    // ignore, this is for better error messages
    debug::set_panic_hook();

    let code = include_str!("test.mar");
    let ast = Parser::new(code);
    println!("Abstract Syntax Tree Output\n\x1b[2m{ast:?}\x1b[0m");
    let mut int = Interpreter::new(&ast);
    println!("\x1b[1mProgram Output\x1b[0m");
    int.interpret();
}
