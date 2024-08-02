use std::io::{Error, Write};
mod lexer;
mod token;
mod location;
// mod command_line;
// use command_line::CommandLineArgs;
mod parser;
use parser::Parser;

fn main() -> Result<(), Error> {
    let cl_args = std::env::args();
    // let mut args = CommandLineArgs::new();
    // for cl_arg in cl_args {
    //     if cl_arg.ends_with(".lox") {
    //         args.source_files.push(cl_arg.to_string());
    //     }
    // }
    // let file = args.source_files.first().expect("[ERROR]: Please provide a source file!").to_string();

    match &cl_args.len() {
        3.. => {

        }
        2 => {
            let file = cl_args.last().unwrap();
            let mut lexer = lexer::Lexer::new();
            let tokens = lexer.lex_file(file);
            let mut parser = Parser::new(tokens);
	    let expr = parser.parse();
	    match expr {
                Ok(e) => {
		    e.print();
		    println!("[INFO][parser] {:?}", e.evaluate());
		}
                Err(m) => println!("[ERROR][parser] {}", m)
            }

       }
        _ => {
            loop {
                let mut input = String::new(); 
                print!("> ");
                let _ = std::io::stdout().flush();
                std::io::stdin().read_line(&mut input).unwrap();

                let mut lexer = lexer::Lexer::new();
                let tokens = lexer.lex_stdin(input);
            
                let mut parser = Parser::new(tokens);
            
                match parser.parse() {
                    Ok(e) => {
			println!("[INFO][parser] {}", e);
			match e.evaluate() {
			    Ok(x) => println!("{}", x),
			    Err(x) => println!("[ERROR][parser] {}", x),
			}
		    },
                    Err(m) => println!("{}", m)
                }
            }
        }
    }


    Ok(())
}

