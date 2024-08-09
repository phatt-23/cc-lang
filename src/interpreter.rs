use crate::{enviroment::Environment, expression::Expr, literal_value::LitVal, loc_error::LocErr, location::Location, statement::Stmt, token::TokenKind};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub struct Interpreter {
    pub globals: Rc<RefCell<Environment>>,
    pub enviroment: Rc<RefCell<Environment>>,
    locals: HashMap<Expr, usize>,
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
                Ok(Some(InterpReturn { location: Location::create("Built_In".to_string(), 0, 0), value: LitVal::Int(time as i32) }))
            })
        };

        global_env.define(String::from("clock"), f);

        let global_env = Rc::new(RefCell::new(global_env));

        Self {
            globals: global_env.clone(),
            enviroment: global_env.clone(),
            locals: HashMap::new(),
        }
    }

    pub fn for_closure(global_env: Rc<RefCell<Environment>>, enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            globals: global_env,
            enviroment: Rc::new(RefCell::new(Environment::new_local(enclosing))),
            locals: HashMap::new(),
        }
    }
}

#[macro_export]
macro_rules! check_return {
    ( $ret:expr ) => {
        match $ret {
            Ok(Some(val)) => return Ok(Some(val)),
            Ok(None) => {},
            Err(err) => return Err(err),
        }
    };
}

impl Interpreter {
    pub fn interpret_with_resolved(&mut self, resolved_locals: HashMap<Expr, usize>, stmts: Vec<Stmt>) -> Result<Option<InterpReturn>, LocErr> {
        self.locals = resolved_locals;

        self.interpret_stmts(stmts)
    }

    pub fn interpret_stmts(&mut self, stmts: Vec<Stmt>) -> Result<Option<InterpReturn>, LocErr> {
        for stmt in stmts {
            match stmt {
                Stmt::Return { keyword, value } => {
                    let evald_value = match value {
                        Some(val) => match self.evaluate(val) {
                            Ok(v) => v,
                            Err(e) => return Err(e)
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
                    // clone this enviroment, for the closure to capture
                    let global_env = self.globals.clone();
                    let parent_env = self.enviroment.clone();
                    let loc = ident.loc().clone();
                    
                    // define the closure
                    let func_impl = move |args: Vec<LitVal>| -> Result<Option<InterpReturn>, LocErr> {
                        let mut function_interp = Interpreter::for_closure(global_env.clone(), parent_env.clone());

                        for (index, arg) in args.iter().enumerate() {
                            let param_ident = match params[index].kind() {
                                TokenKind::Identifier(ident) => ident.clone(),
                                _ => unreachable!("Parameter in the `params` vector should be Identifier.")
                            };
                            function_interp.enviroment.borrow_mut().define(param_ident, arg.clone());
                        }
                        
                        for i in 0..body.len() {
                            check_return!(function_interp.interpret_stmts(vec![body[i].clone()]));
                        }

                        return Ok(Some(InterpReturn { location: loc.clone(), value: LitVal::Nil } ));
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

                    let value = match self.evaluate(expression) {
                        Ok(val) => val,
                        Err(err) => return Err(err)
                    };
                    
                    self.enviroment.borrow_mut().define(ident.clone(), value);
                }
                Stmt::While { condition, body } => {
                    let body = match *body {
                        Stmt::Block { statements } => statements,
                        any => vec![any]
                    };

                    let mut cond = match self.evaluate(condition.clone()) {
                        Ok(value) => value,
                        Err(err) => return Err(err)
                    };

                    while cond.is_truthy() == LitVal::Bool(true) {
                        let a = self.interpret_stmts(body.clone());
                        check_return!(a);
                        
                        cond = match self.evaluate(condition.clone()) {
                            Ok(value) => value,
                            Err(err) => return Err(err)
                        }
                    }

                }
                Stmt::If { condition, then_stmt, else_stmt } => {
                    let cond_expr = self.evaluate(condition);

                    match cond_expr {
                        Ok(cond) => {
                            if matches!(cond.is_truthy(), LitVal::Bool(true)) {
                                check_return!(self.interpret_stmts(vec![*then_stmt]));
                            } else if let Some(else_stmt) = else_stmt {
                                check_return!(self.interpret_stmts(vec![*else_stmt]));
                            }
                        }
                        Err(err) => return Err(err)
                    }
                }
                Stmt::Block { statements } => {
                    let enclosing_env = self.enviroment.clone();
                    let block_env = Environment::new_local(self.enviroment.clone());                    
                    
                    self.enviroment = Rc::new(RefCell::new(block_env));

                    check_return!(self.interpret_stmts(statements));
                    
                    self.enviroment = enclosing_env;
                }
                Stmt::Expression { expression } => {
                    let expr = self.evaluate(expression);
                    
                    match expr {
                        Ok(_) => {}
                        Err(err) => return Err(err)
                    }
                }
                Stmt::Print { expression } => {
                    let expr = self.evaluate(expression);
                    
                    match expr {
                        Ok(val) => println!("{}", val),
                        Err(err) => return Err(err)
                    }
                }
            }
        }

        Ok(None)
    }

    pub fn evaluate(&mut self, expr: Expr) -> Result<LitVal, LocErr> {
    	match expr.clone() {
			Expr::Lambda { params, body, right_paren } => {
				let lambda_indentifier = format!("Lambda_{:x}", rand::random::<usize>());
				let lambda_arity = params.len();
				
				// clone them to this scope for the closure to capture
				let params = params.clone();
				let body = body.clone();
				let loc = right_paren.loc().clone();
                let global_env = self.globals.clone();
                let env = self.enviroment.clone();

				// define the lambda
				let lambda_impl = move |args: Vec<LitVal>| -> Result<Option<InterpReturn>, LocErr> {
					let mut lambda_interp = Interpreter::for_closure(global_env.clone(), env.clone());

					for (index, arg) in args.iter().enumerate() {
						let param_ident = match params[index].kind() {
							TokenKind::Identifier(ident) => ident.clone(),
							_ => unreachable!("Parameter in the `params` vector should be Identifier.")
						};
						lambda_interp.enviroment.borrow_mut().define(param_ident, arg.clone());
					}
					
					for i in 0..body.len() {
						check_return!(lambda_interp.interpret_stmts(vec![body[i].clone()]));
					}

					return Ok(Some(InterpReturn { location: loc.clone(), value: LitVal::Nil } ))
				};

				Ok(LitVal::Callable { 
					ident: lambda_indentifier, 
					arity: lambda_arity, 
					func: Rc::new(lambda_impl)
				})
			}
			Expr::Call { callee, arguments, right_paren } => {
                // if the call fails
                let fmtd_callee = format!("{}", callee);
                let fmtd_args: Vec<_> = arguments.iter().map(|arg| format!("{}", arg)).collect();
                let fmtd_args = fmtd_args.join(", ");

				let callable = self.evaluate(*callee)?;

				match &callable {
                    LitVal::Callable { ident, arity, func } => {

                        if *arity as usize != arguments.len() {
							return Err(LocErr::new(right_paren.loc(), format!("Callable `{}` expects {} arguments, but received {}. Arity check failed.", &ident, &arity, arguments.len())))
						}

						let mut evaled_args = vec![];
						for arg in arguments {
							let evaled_arg = self.evaluate(arg)?;
							evaled_args.push(evaled_arg);
						}  

						let call_result = func(evaled_args);
						match call_result {
							Ok(ok) => Ok(ok.expect("Every callable should at least return Nil. Every function returns nil implicitly.").value),
							Err(err) => {
								Err(LocErr::new(right_paren.loc(), format!("Failed execution of callable `{}` with ({}) as passed arguments.\n  [ERROR-from-callable][{}] {}", &ident, fmtd_args, err.loc, err.msg)))
							}
						}
					}
					any => Err(LocErr::new(right_paren.loc(), format!("Trying to call `{}` of value {}, which is not callable.", fmtd_callee, any)))
				}
			}
    	    Expr::Literal { value } => Ok(value.clone()),
    	    Expr::Grouping { expr } => self.evaluate(*expr),
    	    Expr::Unary { operator, expr } => {
        		let right = self.evaluate(*expr)?;
        		match (right, operator.kind()) {
        		    (LitVal::Int(x), TokenKind::Minus) => Ok(LitVal::Int(-x)),
        		    (LitVal::Double(x), TokenKind::Minus) => Ok(LitVal::Double(-x)),
        		    (any, TokenKind::Minus) => Err(LocErr::new(operator.loc(), format!("Minus operator not implemented for {:?}", any))),
        		    (any, TokenKind::Bang) => Ok(any.is_falsy()),
        		    _ => todo!()
        		}
    	    }
            Expr::Assign { target, value } => {
                if let TokenKind::Identifier(i) = target.kind() {
                    let v = self.evaluate(*value)?;
                    return match self.enviroment.as_ref().borrow_mut().assign(i, v.clone()) {
                        Some(_) => Ok(v),
                        None => Err(LocErr::new(target.loc(), format!("Can't assign {} to an undeclared target `{}`.", v, i)))
                    }
                }
                unreachable!("{} Target must be an identifier", target.loc());
            }
            Expr::Var { ident } => {
				if let TokenKind::Identifier(name) = ident.kind() {
                    if let Some(val) = self.look_up_variable(name, &expr) {
                        return Ok(val)
                    }

					match self.enviroment.as_ref().borrow().get(name) {
						Some(value) => return Ok(value.clone()),
						None => return Err(LocErr::new(ident.loc(), format!("Variable with identifier `{}` is undeclared.", name)))
					}
				}
				unreachable!("{} Totally fucked up. {:?} is not an identifier.", ident.loc(), ident.kind());
            }
            Expr::Logical { operator, left, right } => {
                let left = self.evaluate(*left.clone())?;
                match operator.kind() {
					TokenKind::And => {
                        if left.is_truthy() == LitVal::Bool(false) {
                            return Ok(left);
                        }
                    }
                    TokenKind::Or => {
                        if left.is_truthy() == LitVal::Bool(true) {
                            return Ok(left)
                        }
                    }
                    _ => unreachable!("Can't perform a logical evaluation with non-logical operations. {:?}", operator)
                }

                Ok(self.evaluate(*right)?)
            }
    	    Expr::Binary { operator, left, right } => {
        		use LitVal::*;
        		let left = self.evaluate(*left)?;
        		let right = self.evaluate(*right)?;

        		match operator.kind() {
        		    TokenKind::Minus => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Double(x - y)),
            			    (Int(x), Int(y)) => Ok(Int(x - y)),
            			    (Double(x), Int(y)) => Ok(Int(x as i32 - y)),
            			    (Int(x), Double(y)) => Ok(Int(x - y as i32)),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical values.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::Plus => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Double(x + y)),
            			    (Int(x), Int(y)) => Ok(Int(x + y)),
            			    (Double(x), Int(y)) => Ok(Int(x as i32 + y)),
            			    (Int(x), Double(y)) => Ok(Int(x + y as i32)),
            			    (String(x), String(y)) => Ok(String(format!("{}{}", x, y))),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical or concatenable.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::Star => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Double(x * y)),
            			    (Int(x), Int(y)) => Ok(Int(x * y)),
            			    (Double(x), Int(y)) => Ok(Double(x * f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Double(f64::from(x) * y)),
            			    (l, r) => {
								Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
							}
            			}
        		    }
        		    TokenKind::Slash => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Double(x / y)),
            			    (Int(x), Int(y)) => Ok(Int(x / y)),
            			    (Double(x), Int(y)) => Ok(Double(x / f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Double(f64::from(x) / y)),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::Greater => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Bool(x > y)),
            			    (Int(x), Int(y)) => Ok(Bool(x > y)),
            			    (Double(x), Int(y)) => Ok(Bool(x > f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Bool(f64::from(x) > y)),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::Less => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Bool(x < y)),
            			    (Int(x), Int(y)) => Ok(Bool(x < y)),
            			    (Double(x), Int(y)) => Ok(Bool(x < f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Bool(f64::from(x) < y)),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::GreaterEqual => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Bool(x >= y)),
            			    (Int(x), Int(y)) => Ok(Bool(x >= y)),
            			    (Double(x), Int(y)) => Ok(Bool(x >= f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Bool(f64::from(x) >= y)),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::LessEqual => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Bool(x <= y)),
            			    (Int(x), Int(y)) => Ok(Bool(x <= y)),
            			    (Double(x), Int(y)) => Ok(Bool(x <= f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Bool(f64::from(x) <= y)),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::EqualEqual => {
            			match (left, right) {
            			    (Bool(x), Bool(y)) => Ok(Bool(x == y)),
            			    (Double(x), Double(y)) => Ok(Bool(x == y)),
            			    (Int(x), Int(y)) => Ok(Bool(x == y)),
            			    (Double(x), Int(y)) => Ok(Bool(x == f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Bool(f64::from(x) == y)),
            			    (String(x), String(y)) => Ok(Bool(x == y)),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical or boolean", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::BangEqual => {
            			match (left, right) {
            			    (Bool(x), Bool(y)) => Ok(Bool(x != y)),
            			    (Double(x), Double(y)) => Ok(Bool(x != y)),
             			    (Int(x), Int(y)) => Ok(Bool(x != y)),
            			    (Double(x), Int(y)) => Ok(Bool(x != f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Bool(f64::from(x) != y)),
            			    (String(x), String(y)) => Ok(Bool(x != y)),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on non-numerical or non-boolean values {} and {}", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::And | TokenKind::Or => unreachable!("Logical operations are not no be processed by the Binary branch."),
        		    e => Err(LocErr::new(operator.loc(), format!("Undefined binary operator {}.", e)))
        		}
    	    }
    	}
    }

    fn look_up_variable(&mut self, name: &String, expr: &Expr) -> Option<LitVal> {
        let distance = self.locals.get(expr);
        match distance {
            Some(dist) => self.enviroment.borrow().get_at(dist, name),
            None => self.globals.borrow().get(name)
        }
    }
}
