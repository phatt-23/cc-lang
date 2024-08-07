use crate::{enviroment::Environment, literal_value::LitVal, loc_error::LocErr, location::Location, statement::Stmt, token::TokenKind};
use std::{cell::RefCell, rc::Rc};

pub struct Interpreter {
    pub enviroment: Rc<RefCell<Environment>>,
}

// example function
#[derive(Debug)]
pub struct InterpReturn {
    pub location: Location,
    pub value: LitVal,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut global_env = Environment::new();
        
        let f = LitVal::Callable { 
            ident: String::from("clock"), 
            arity: 0, 
            func: Rc::new(|_| {
                let time = std::time::SystemTime::now().duration_since(std::time::SystemTime::UNIX_EPOCH).unwrap().as_secs();
                Ok(LitVal::Int(time as i32))
            })
        };

        global_env.define(String::from("clock"), f);
        
        Self {
            enviroment: Rc::new(RefCell::new(global_env)),
        }
    }

    pub fn for_closure(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            enviroment: Rc::new(RefCell::new(Environment::new_local(enclosing)))
        }
    }

    /// result < ok, err > 
    /// Ok(())          ... Successful interpreting of anything other than return
    /// Err(value)      ... Returning return stmt's value as an error
    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<Option<InterpReturn>, LocErr> {
        fn print_err(err: &LocErr) {
            println!("[ERROR][interp][{}] {}", err.loc, err.msg);
        }

        for stmt in stmts {
            match stmt {
                Stmt::Return { keyword, value } => {
                    let evald_value = match value {
                        Some(val) => match val.evaluate(self.enviroment.clone()) {
                            Ok(v) => v,
                            Err(e) => {
                                // print_err(&e);
                                return Err(e)
                            }
                        }
                        None => LitVal::Nil,
                    };
                    
                    // This need to be cathed by the function it was called from
                    return Ok(Some(InterpReturn { location: keyword.loc().clone(), value: evald_value })) 
                }
                Stmt::Function { ident, params, body } => {
                    let function_identifer = match ident.kind() {
                        TokenKind::Identifier(value) => value,
                        _ => unreachable!("{} Can't use {:?} as a function identifier.", ident.loc(), ident.kind())
                    };
                    
                    // get the length beforehand, because the params are moved to the closure
                    let params_len = params.len();

                    let parent_env = self.enviroment.clone();
                    // define the closure
                    let func_impl = move |args: Vec<LitVal>| -> Result<LitVal, LocErr> {
                        let mut function_interp = Interpreter::for_closure(parent_env.clone());

                        for (index, arg) in args.iter().enumerate() {
                            let param_ident = match params[index].kind() {
                                TokenKind::Identifier(ident) => ident.clone(),
                                _ => unreachable!("Parameter in the `params` vector should be Identifier.")
                            };
                            function_interp.enviroment.borrow_mut().define(param_ident, arg.clone());
                        }
                        
                        for i in 0..body.len() {
                            let interp_result = function_interp.interpret(vec![body[i].clone()]);

                            match interp_result {
                                Ok(opt_ret) => match opt_ret {
                                    Some(ret) => return Ok(ret.value),
                                    None => {}, // Do nothing
                                }
                                Err(err) => return Err(err)
                            }
                        }

                        return Ok(LitVal::Nil)
                    };

                    let function_declaration = LitVal::Callable { 
                       ident: function_identifer.clone(), 
                       arity: params_len, 
                       func: Rc::new(func_impl),
                    };

                    self.enviroment.borrow_mut().define(function_identifer.clone(), function_declaration);
                }
                Stmt::Var { ident, expression } => {
                    let ident = match ident.kind() {
                        TokenKind::Identifier(value) => value,
                        _ => unreachable!("{} Can't use {:?} as a variable identifier.", ident.loc(), ident.kind())
                    };

                    let value = match expression.evaluate(self.enviroment.clone()) {
                        Ok(val) => val,
                        Err(err) => {
                            // print_err(&err);
                            // continue;
                            return Err(err);
                        }
                    };
                    
                    self.enviroment.borrow_mut().define(ident.clone(), value);
                }
                Stmt::While { condition, body } => {
                    let body = match *body {
                        Stmt::Block { statements } => statements,
                        any => vec![any]
                    };

                    let mut cond = match condition.evaluate(self.enviroment.clone()) {
                        Ok(value) => value,
                        Err(err) => {
                            // print_err(&err);
                            // continue;
                            return Err(err);
                        }
                    };

                    while cond.is_truthy() == LitVal::Bool(true) {
                        self.interpret(body.clone())?;
                        
                        cond = match condition.evaluate(self.enviroment.clone()) {
                            Ok(value) => value,
                            Err(err) => {
                                // print_err(&err);
                                return Err(err);
                            }
                        }
                    }

                }
                Stmt::If { condition, then_stmt, else_stmt } => {
                    let cond_expr = condition.evaluate(self.enviroment.clone());

                    match cond_expr {
                        Ok(cond) => {
                            if matches!(cond.is_truthy(), LitVal::Bool(true)) {
                                self.interpret(vec![*then_stmt])?;
                            } else if let Some(else_stmt) = else_stmt {
                                self.interpret(vec![*else_stmt])?;
                            }
                        }
                        Err(err) => {
                            // print_err(&err);
                            // continue;
                            return Err(err);
                        }
                    }
                }
                Stmt::Block { statements } => {
                    let enclosing_env = self.enviroment.clone();
                    let block_env = Environment::new_local(self.enviroment.clone());                    
                    
                    self.enviroment = Rc::new(RefCell::new(block_env));

                    self.interpret(statements)?;
                    
                    self.enviroment = enclosing_env;
                }
                Stmt::Expression { expression } => {
                    let expr = expression.evaluate(self.enviroment.clone());
                    
                    match expr {
                        Ok(_) => {}
                        Err(err) => {
                            // print_err(&err);
                            // continue;
                            return Err(err);
                        }
                    }
                }
                Stmt::Print { expression } => {
                    let expr = expression.evaluate(self.enviroment.clone());
                    
                    match expr {
                        Ok(val) => println!("{}", val),
                        Err(err) => {
                            // print_err(&err);
                            // continue;
                            return Err(err);
                        }
                    }
                }
            }
        }

        Ok(None)
    }

}
