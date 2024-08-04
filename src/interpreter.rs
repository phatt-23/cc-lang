use crate::{enviroment::Enviroment, expression::LitVal, loc_error::LocErr, statement::Stmt, token::TokenKind};
use std::{borrow::BorrowMut, rc::Rc};

pub struct Interpreter {
    enviroment: Box<Enviroment>,
}

impl Interpreter {
    pub fn new() -> Self {
	    Self {
            enviroment: Box::new(Enviroment::new()),
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) {
        fn print_err(err: &LocErr) {
            println!("[ERROR][interp] {} {}", err.loc, err.msg);
        }

        let mut errs: Vec<LocErr> = Vec::new();

        for stmt in stmts {
            match stmt {
                Stmt::While { condition, body } => {
                    let condition_result = condition.evaluate(&mut self.enviroment);

                    let body = match *body {
                        Stmt::Block { statements } => statements,
                        any => vec![any]
                    };
                    
                    match condition_result {
                        Ok(mut cond) => {
                            while cond.is_truthy() == LitVal::Bool(true) {
                                self.interpret(body.clone());
                                match condition.evaluate(&mut self.enviroment) {
                                    Ok(new_cond) => cond = new_cond,
                                    Err(err) => {
                                        errs.push(err);
                                        continue;
                                    }
                                }
                            }
                        }
                        Err(err) => {
                            print_err(&err);
                            errs.push(err);
                            continue;
                        }
                    }

                }
                Stmt::If { condition, then_stmt, else_stmt } => {
                    let cond_expr = condition.evaluate(&mut self.enviroment);
                    match cond_expr {
                        Ok(cond) => {
                            if matches!(cond.is_truthy(), LitVal::Bool(true)) {
                                self.interpret(vec![*then_stmt])
                            } else if let Some(else_stmt) = else_stmt {
                                self.interpret(vec![*else_stmt])
                            }
                        }
                        Err(err) => {
                            print_err(&err);
                            errs.push(err);
                            continue;
                        }
                    }
                }
                Stmt::Block { statements } => {
                    //  inner scope                                 // outer scope as .enclosing
                    let local_env = Box::from(Enviroment::new_local(self.enviroment.clone()));
                    
                    // swaping current enviroment with local env
                    self.enviroment = local_env;
                    
                    // recusively interpret
                    self.interpret(statements);

                    // go back 
                    let new_enviroment = *self.enviroment.enclosing.clone().expect("There must be an enclosing (outer) scope to this local enviroment.");
                    self.enviroment = Box::from(new_enviroment);
                }
                Stmt::Expression { expression } => {
                    let expr = expression.evaluate(&mut self.enviroment);
                    
                    match expr {
                        Ok(_) => {}
                        Err(err) => {
                            print_err(&err);
                            errs.push(err)
                        }
                    }
                }
                Stmt::Print { expression } => {
                    let expr = expression.evaluate(&mut self.enviroment);
                    
                    match expr {
                        Ok(val) => println!("{}", val),
                        Err(err) => {
                            print_err(&err);
                            errs.push(err)
                        }
                    }
                }
                Stmt::Var { ident, expression } => {
                    match ident.kind() {
                        TokenKind::Identifier(i) => {
                            let env = &mut self.enviroment;

                            let expr = expression.evaluate(env);

                            match expr {
                                Ok(val) => env.define(i.clone(), val),
                                Err(err) => {
                                    print_err(&err);
                                    errs.push(err);
                                }
                            }
                        }
                        _ => unreachable!("{} Can't use {:?} as a variable identifier.", ident.loc(), ident.kind())
                    }
                }
            }
        }

        /* 
        TODO: Probably should print the errors as they come in the REPL mode.
        TODO: Only in compilation mode is it a good idea to first collect the error messages.
        TODO: It is never a good idea.
        for e in errs {
            println!("[ERROR][interp] {} {}", e.loc, e.msg);
        }
        */ 
        
    }

}
