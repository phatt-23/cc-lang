use crate::{enviroment::Enviroment, statement::Stmt, token::TokenKind};

pub struct Interpreter {
    enviroment: Enviroment,
}

impl Interpreter {
    pub fn new() -> Self {
	    Self {
            enviroment: Enviroment::new(),
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<(), String> {
        for stmt in stmts {
            match stmt {
                Stmt::Expression { expression } => {
                    expression.evaluate(&mut self.enviroment)?;
                }
                Stmt::Print { expression } => {
                    let value = expression.evaluate(&mut self.enviroment)?;
                    println!("{}", value);
                }
                Stmt::Var { ident, expression } => {
                    match ident.kind() {
                        TokenKind::Identifier(i) => {
                            let value = expression.evaluate(&mut self.enviroment)?;
                            self.enviroment.define(i.clone(), value);
                            return Ok(())
                        }
                        _ => return Err(format!("{} Can't use {:?} as a variable identifier.", ident.loc(), ident.kind()))
                    }
                }
            }
        }
        
        Ok(())
    }

}
