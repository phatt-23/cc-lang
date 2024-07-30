use std::io::Error;
mod lexer;
mod token;
mod location;
mod command_line;
mod parser;

use parser::Parser;

fn main() -> Result<(), Error> {
    let cl_args = std::env::args();
    let mut args: command_line::CommandLineArgs = command_line::CommandLineArgs::new();
    for cl_arg in cl_args {
        if cl_arg.ends_with(".lox") {
            args.source_files.push(cl_arg.to_string());
        }
    }
    let file = args.source_files.first().expect("[ERROR]: Please provide a source file!").to_string();

    let mut lexer = lexer::Lexer::new();
    let tokens = lexer.lex_file(file);

    let mut parser = Parser::new(tokens);
    parser.parse().print();

    Ok(())
}

