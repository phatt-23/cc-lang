use crate::{enviroment::Enviroment, literal_value::LitVal, loc_error::LocErr, statement::Stmt, token::TokenKind};
use std::rc::Rc;

pub struct Interpreter {
    enviroment_stack: Vec<Enviroment>,
}

// example function
fn clock_native_func(_args: Vec<LitVal>) -> LitVal {
    let time = std::time::SystemTime::now().elapsed().unwrap().as_micros();
    LitVal::Int(time as i32)
}

impl Interpreter {
    pub fn new() -> Self {
        let mut global_env = Enviroment::new();
        
        let f = LitVal::Callable { 
            ident: String::from("clock"), 
            arity: 0, 
            func: Rc::new(clock_native_func) 
        };

        global_env.define(String::from("clock"), f);
        
        Self {
            enviroment_stack: vec![global_env],
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) {
        fn print_err(err: &LocErr) {
            println!("[ERROR][interp] {} {}", err.loc, err.msg);
        }

        for stmt in stmts {
            match stmt {
                Stmt::While { condition, body } => {
                    let current_env = self.enviroment_stack.last_mut().expect("There must be current enviroment, at least one enviroment, the global one.");
                    let condition_result = condition.evaluate(current_env);

                    let body = match *body {
                        Stmt::Block { statements } => statements,
                        any => vec![any]
                    };
                    
                    match condition_result {
                        Ok(mut cond) => {
                            while cond.is_truthy() == LitVal::Bool(true) {
                                self.interpret(body.clone());
                                let current_env = self.enviroment_stack.last_mut().expect("There must be current enviroment, at least one enviroment, the global one.");
                                
                                match condition.evaluate(current_env) {
                                    Ok(new_cond) => cond = new_cond,
                                    Err(err) => {
                                        print_err(&err);
                                        continue;
                                    }
                                }
                            }
                        }
                        Err(err) => {
                            print_err(&err);
                            continue;
                        }
                    }

                }
                Stmt::If { condition, then_stmt, else_stmt } => {
                    let current_env = self.enviroment_stack.last_mut().expect("There must be current enviroment, at least one enviroment, the global one.");

                    let cond_expr = condition.evaluate(current_env);
                    match cond_expr {
                        Ok(cond) => {
                            // shortcicuiting
                            if matches!(cond.is_truthy(), LitVal::Bool(true)) {
                                self.interpret(vec![*then_stmt])
                            } else if let Some(else_stmt) = else_stmt {
                                self.interpret(vec![*else_stmt])
                            }
                        }
                        Err(err) => {
                            print_err(&err);
                            continue;
                        }
                    }
                }
                Stmt::Block { statements } => {
                    let current_env = self.enviroment_stack.pop().expect("There must be current enviroment, at least one enviroment, the global one.");
                    // crate the local inner enviroment with 
                    // push local env with current env onto stack
                    let local_env = Enviroment::new_local(Box::from(current_env));
                    self.enviroment_stack.push(local_env);

                    self.interpret(statements);

                    let local_env = self.enviroment_stack.pop().expect("Should be unreachable. There's no enviroment to pop.");
                    
                    // the the old enviroment back to the stack
                    // check that the stack is at least filled with the global enviroment
                    self.enviroment_stack.push(*local_env.enclosing.expect("The enclosing enviroment should be there we just set it."));
                    assert!(!self.enviroment_stack.is_empty(), "Can't pop the global enviroment. Now the enviroment stack is empty.");
                }
                Stmt::Expression { expression } => {
                    let current_env = self.enviroment_stack.last_mut().expect("There must be current enviroment, at least one enviroment, the global one.");
                    let expr = expression.evaluate(current_env);
                    
                    match expr {
                        Ok(_) => {}
                        Err(err) => {
                            print_err(&err);
                        }
                    }
                }
                Stmt::Print { expression } => {
                    let current_env = self.enviroment_stack.last_mut().expect("There must be current enviroment, at least one enviroment, the global one.");
                    let expr = expression.evaluate(current_env);
                    
                    match expr {
                        Ok(val) => println!("{}", val),
                        Err(err) => {
                            print_err(&err);
                        }
                    }
                }
                Stmt::Var { ident, expression } => {
                    match ident.kind() {
                        TokenKind::Identifier(i) => {
                            let current_env = self.enviroment_stack.last_mut().expect("There must be current enviroment, at least one enviroment, the global one.");

                            let expr = expression.evaluate(current_env);

                            match expr {
                                Ok(val) => current_env.define(i.clone(), val),
                                Err(err) => {
                                    print_err(&err);
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
