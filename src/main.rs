mod lexer;
mod token;
mod location;
mod parser;
mod interpreter;
mod statement;
mod expression;
mod enviroment;

use std::{fs, io::{Error, Write}};
use parser::Parser;
use lexer::Lexer;
use interpreter::Interpreter;

fn main() -> Result<(), Error> {
    let cl_args = std::env::args();
    let mut interpreter = Interpreter::new();

    match &cl_args.len() {
        3.. => {}
        2 => {
            let filepath = cl_args.last().unwrap();
	        let source: String = fs::read_to_string(&filepath).unwrap();
            run(&mut interpreter, filepath, source).unwrap_or_else(|msg| { println!("[ERROR][interpreter] {}", msg); });
        }
        _ => {
            loop {
                let mut input = String::new(); 
                print!(">>> ");
                let _ = std::io::stdout().flush();
                std::io::stdin().read_line(&mut input).unwrap();
                run(&mut interpreter, "STDIN".to_string(), input).unwrap_or_else(|msg| { println!("[ERROR][interpreter] {}", msg); });
            }
        }
    }

    Ok(())
}

fn run(interpreter: &mut Interpreter, filepath: String, source: String) -> Result<(), String> {
    let mut lexer = Lexer::new();
    let mut parser = Parser::new();

    let tokens = lexer.lex(filepath, source);
    let stmts = parser.parse(tokens)?;
    interpreter.interpret(stmts)?;

    Ok(())
}
