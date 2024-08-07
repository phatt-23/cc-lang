mod lexer;
mod token;
mod location;
mod parser;
mod interpreter;
mod statement;
mod expression;
mod enviroment;
mod loc_error;
mod literal_value;

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
            run(&mut interpreter, filepath, source);
        }
        _ => {
            loop {
                let mut input = String::new(); 
                print!(">>> ");
                let _ = std::io::stdout().flush();
                std::io::stdin().read_line(&mut input).unwrap();
                run(&mut interpreter, "STDIN".to_string(), input);
            }
        }
    }

    Ok(())
}

fn run(interpreter: &mut Interpreter, filepath: String, source: String) {
    let mut lexer = Lexer::new();
    let mut parser = Parser::new();

    let tokens = lexer.lex(filepath, source);

    let stmts = parser.parse(tokens);

    let interp_result = interpreter.interpret(stmts);

    match interp_result {
        Ok(opt_ret) => match opt_ret {
            Some(ret) => println!("[ERROR][interpreter] {} Called `return` at global level with value {}.", ret.location, ret.value),
            None => {}
        }
        Err(err) => println!("[ERROR][interpreter][{}] {}", err.loc, err.msg)
    }

}
