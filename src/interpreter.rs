use crate::{enviroment::Enviroment, literal_value::LitVal, loc_error::LocErr, statement::Stmt, token::{Token, TokenKind}};
use std::{cell::RefCell, rc::Rc};

pub struct Interpreter {
    pub enviroment: Rc<RefCell<Enviroment>>,
}

// example function

impl Interpreter {
    pub fn new() -> Self {
        let mut global_env = Enviroment::new();
        
        let f = LitVal::Callable { 
            ident: String::from("clock"), 
            arity: 0, 
            func: Rc::new(|_, _| -> LitVal {
                let time = std::time::SystemTime::now().duration_since(std::time::SystemTime::UNIX_EPOCH).unwrap().as_secs();
                LitVal::Int(time as i32)
            })
        };

        global_env.define(String::from("clock"), f);
        
        Self {
            enviroment: Rc::new(RefCell::new(global_env)),
        }
    }

    pub fn new_closure(enclosing: Rc<RefCell<Enviroment>>) -> Self {
        Self {
            enviroment: Rc::new(RefCell::new(Enviroment::new_local(enclosing)))
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) {
        fn print_err(err: &LocErr) {
            println!("[ERROR][interp] {} {}", err.loc, err.msg);
        }

        for stmt in stmts {
            match stmt {
                Stmt::Function { ident, params, body } => {
                    let function_identifer = match ident.kind() {
                        TokenKind::Identifier(value) => value,
                        _ => unreachable!("{} Can't use {:?} as a function identifier.", ident.loc(), ident.kind())
                    };
                    
                    // deep copy of params and body
                    let parameters: Vec<Token> = params.iter().map(|p| p.clone()).collect();
                    let statements: Vec<Box<Stmt>> = body.iter().map(|b| b.clone()).collect(); 

                    // define the closure
                    let func_impl = move |parent_env: Rc<RefCell<Enviroment>>, args: Vec<LitVal>| -> LitVal {
                        let mut closure_interp = Interpreter::new_closure(parent_env);

                        for (index, arg) in args.iter().enumerate() {
                            let param_ident = match params[index].kind() {
                                TokenKind::Identifier(ident) => ident.clone(),
                                _ => unreachable!("Parameter in the `params` vector should be Identifier.")
                            };
                            closure_interp.enviroment.borrow_mut().define(param_ident, arg.clone());
                        }
                        
                        for i in 0..body.len() {
                            closure_interp.interpret(vec![*statements[i].clone()]);
                        }

                        return LitVal::Nil
                    };

                    let function_declaration = LitVal::Callable { 
                       ident: function_identifer.clone(), 
                       arity: parameters.len(), 
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
                            print_err(&err);
                            continue;
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
                            print_err(&err);
                            continue;
                        }
                    };
                    
                    while cond.is_truthy() == LitVal::Bool(true) {
                        self.interpret(body.clone());
                        
                        cond = match condition.evaluate(self.enviroment.clone()) {
                            Ok(value) => value,
                            Err(err) => {
                                print_err(&err);
                                continue;
                            }
                        }
                    }

                }
                Stmt::If { condition, then_stmt, else_stmt } => {
                    let cond_expr = condition.evaluate(self.enviroment.clone());

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
                            continue;
                        }
                    }
                }
                Stmt::Block { statements } => {
                    let enclosing_env = self.enviroment.clone();
                    let block_env = Enviroment::new_local(self.enviroment.clone());
                    
                    self.enviroment = Rc::new(RefCell::new(block_env));

                    self.interpret(statements);
                    
                    self.enviroment = enclosing_env;
                }
                Stmt::Expression { expression } => {
                    let expr = expression.evaluate(self.enviroment.clone());
                    
                    match expr {
                        Ok(_) => {}
                        Err(err) => {
                            print_err(&err);
                        }
                    }
                }
                Stmt::Print { expression } => {
                    let expr = expression.evaluate(self.enviroment.clone());
                    
                    match expr {
                        Ok(val) => println!("{}", val),
                        Err(err) => {
                            print_err(&err);
                        }
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
