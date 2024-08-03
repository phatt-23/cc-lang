use crate::statement::Stmt;

pub struct Interpreter {

}

impl Interpreter {
    pub fn new() -> Self {
	    Self {
	    }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<(), String> {
        for stmt in stmts {
            match stmt {
                Stmt::Expression { expression } => {
                    expression.evaluate()?;
                }
                Stmt::Print { expression } => {
                    let value = expression.evaluate()?;
                    println!("{}", value);
                }
            }
        }
        
        Ok(())
    }

}
