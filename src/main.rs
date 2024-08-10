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
mod resolver;
mod callable_entity;
mod runtime_entity;
mod class_entity;
mod function_entity;

use std::{fs, io::{Error, Write}};
use parser::Parser;
use lexer::Lexer;
use interpreter::Interpreter;
use resolver::Resolver;

fn main() -> Result<(), Error> {
    let cl_args = std::env::args();

    match &cl_args.len() {
        3.. => {}
        2 => {
            let filepath = cl_args.last().unwrap();
	        let source: String = fs::read_to_string(&filepath).unwrap();
            
            let mut main = Main::new();
            match main.run(filepath, source) {
                Ok(_) => return Ok(()),
                Err(msg) => println!("{}", msg)
            }
        }
        _ => {
            loop {
                let mut input = String::new(); 
                print!(">>> ");
                let _ = std::io::stdout().flush();
                std::io::stdin().read_line(&mut input).unwrap();

                let mut main = Main::new();
                match main.run(String::from("stdin"), input) {
                    Ok(_) => {},
                    Err(msg) => {
                        println!("{}", msg);
                        break;
                    }
                }
            }
        }
    }

    Ok(())
}

struct Main {
    lexer: Lexer,
    parser: Parser,
    resolver: Resolver,
    interpreter: Interpreter,
}

impl Main {
    fn new() -> Self {
        Self {
            lexer: Lexer::new(),
            parser: Parser::new(),
            resolver: Resolver::new(),
            interpreter: Interpreter::new(),
        }
    }
    
    fn run(&mut self, filepath: String, source: String) -> Result<(), String> {
        let tokens = self.lexer.lex(filepath, source)?;
        let stmts = self.parser.parse(tokens)?;
        
        let resolved_locals = self.resolver.resolve(&stmts)?;
        //dbg!(&resolved_locals);
        let interp_result = self.interpreter.interpret_with_resolved(resolved_locals, stmts);
        match interp_result {
            Ok(opt_ret) => match opt_ret {
                Some(ret) => println!("[ERROR][Interpreter] Called `return` at global level with value {}.", ret),
                None => {}
            }
            Err(err) => println!("[ERROR][Interpreter][{}] {}", err.loc, err.msg)
        }
        
        Ok(())
    }
    
}

