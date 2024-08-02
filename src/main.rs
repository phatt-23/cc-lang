// mod command_line;
// use command_line::CommandLineArgs;
use std::io::{Error, Write};
mod lexer;
use crate::lexer::Lexer;
mod token;
mod location;
mod parser;
use parser::Parser;
mod interpreter;
use interpreter::Interpreter;

fn main() -> Result<(), Error> {
    // let mut args = CommandLineArgs::new();
    // for cl_arg in cl_args {
    //     if cl_arg.ends_with(".lox") {
    //         args.source_files.push(cl_arg.to_string());
    //     }
    // }
    // let file = args.source_files.first().expect("[ERROR]: Please provide a source file!").to_string();

    let cl_args = std::env::args();
    match &cl_args.len() {
        3.. => {}
        2 => {
            let file = cl_args.last().unwrap();
            let mut lexer = Lexer::new();
            let mut parser = Parser::new();
            let tokens = lexer.lex_file(file);
	    let expr = parser.parse(tokens);
	    match expr {
                Ok(e) => {
		    e.print();
		    println!("[INFO][parser] {:?}", e.evaluate());
		}
                Err(m) => println!("[ERROR][parser] {}", m)
            }

       }
        _ => {
            let mut lexer = Lexer::new();
            let mut parser = Parser::new();
	    let mut interpreter = Interpreter::new();
            loop {
                let mut input = String::new(); 
                print!("> ");
                let _ = std::io::stdout().flush();
                std::io::stdin().read_line(&mut input).unwrap();

                let tokens = lexer.lex_stdin(input);
                match parser.parse(tokens) {
                    Ok(e) => {
			println!("[INFO][parser][echo] {}", e);
			match interpreter.interpret(e) {
			    Ok(x) => println!("[INFO][interpreter] {}", x),
			    Err(x) => println!("[ERROR][interpreter] {}", x),
			}
		    },
                    Err(m) => println!("[ERROR][parser] {}", m)
                }
            }
        }
    }


    Ok(())
}

