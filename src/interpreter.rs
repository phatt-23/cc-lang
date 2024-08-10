use crate::{callable_entity::{Callable, CallableEntity}, class_entity::Class, enviroment::Environment, expression::Expr, function_entity::Function, literal_value::LitVal, loc_error::LocErr, runtime_entity::RuntimeEntity, statement::Stmt, token::TokenKind};

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub struct Interpreter {
    pub globals: Rc<RefCell<Environment>>,
    pub enviroment: Rc<RefCell<Environment>>,
    locals: HashMap<Expr, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut global_env = Environment::new();
        
        let f = Function::new(String::from("clock"), 0, 
            Rc::new(|_| {
                let time = std::time::SystemTime::now().duration_since(std::time::SystemTime::UNIX_EPOCH).unwrap().as_secs();
                Ok(Some(RuntimeEntity::Literal(LitVal::Int(time as i32))))
            })
        );

        global_env.define(String::from("clock"), RuntimeEntity::new_from(f));

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
    pub fn interpret_with_resolved(&mut self, resolved_locals: HashMap<Expr, usize>, stmts: Vec<Stmt>) -> Result<Option<RuntimeEntity>, LocErr> {
        self.locals = resolved_locals;

        self.interpret_stmts(stmts)
    }

    pub fn interpret_stmts(&mut self, stmts: Vec<Stmt>) -> Result<Option<RuntimeEntity>, LocErr> {
        for stmt in stmts {
            match stmt {
                Stmt::Class { ident, methods } => {
                    let name = match ident.kind() {
                        TokenKind::Identifier(name) => name.clone(),
                        _ => unreachable!("Class name must be an identifier.")  
                    };
                    
                    let klass = Class::new(name.clone());
                    self.enviroment.borrow_mut().define(name, RuntimeEntity::new_from(klass));
                }
                Stmt::Return { keyword, value } => {
                    let evald_value = match value {
                        Some(val) => match self.evaluate(val) {
                            Ok(v) => v,
                            Err(e) => return Err(LocErr::new(keyword.loc(), format!("Return value failed to evaluate.\n\t    [ERROR-from-return-value][{}] {}", e.loc, e.msg)))
                        }
                        None => RuntimeEntity::new_from(LitVal::Nil),
                    };
                    // This need to be cathed by the function it was called from
                    return Ok(Some(evald_value))
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
                    
                    // define the closure
                    let func_impl = move |args: Vec<RuntimeEntity>| -> Result<Option<RuntimeEntity>, LocErr> {
                        let mut function_interp = Interpreter::for_closure(global_env.clone(), parent_env.clone());

                        for (index, arg) in args.iter().enumerate() {
                            let param_ident = match params[index].kind() {
                                TokenKind::Identifier(ident) => ident.clone(),
                                _ => unreachable!("Parameter in the `params` vector should be Identifier.")
                            };
                            function_interp.enviroment.borrow_mut()
                                .define(param_ident, arg.clone());
                        }
                        
                        for i in 0..body.len() {
                            check_return!(function_interp.interpret_stmts(vec![body[i].clone()]));
                        }

                        return Ok(Some(RuntimeEntity::new_from(LitVal::Nil)));
                    };

                    let function_declaration = Function::new(
                        function_identifer.clone(), 
                        params_len, 
                        Rc::new(func_impl),
                    );

                    self.enviroment.borrow_mut()
                        .define(function_identifer.clone(), RuntimeEntity::new_from(function_declaration));
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
                        Ok(res) => match res {
                            RuntimeEntity::Literal(val) => val,
                            RuntimeEntity::ClassInstance(inst) => todo!("Condition must evaluate to a literal value. Instead got {}.", inst),
                            RuntimeEntity::Callable(f) => todo!("Condition must evaluate to a literal value. Insted got {}.", f)
                        },
                        Err(err) => return Err(err)
                    };

                    while cond.is_truthy() == LitVal::Bool(true) {
                        let a = self.interpret_stmts(body.clone());
                        check_return!(a);
                        
                        cond = match self.evaluate(condition.clone()) {
                            Ok(res) => match res {
                                RuntimeEntity::Literal(val) => val,
                                RuntimeEntity::ClassInstance(inst) => todo!("Condition must evaluate to a literal value. Instead got {}.", inst),
                                RuntimeEntity::Callable(f) => todo!("Condition must evaluate to a literal value. Insted got {}.", f)
                            },
                            Err(err) => return Err(err)
                        }
                    }

                }
                Stmt::If { condition, then_stmt, else_stmt } => {
                    let cond_expr = self.evaluate(condition);

                    match cond_expr {
                        Ok(res) => match res {
                            RuntimeEntity::Literal(val) => {
                                if matches!(val.is_truthy(), LitVal::Bool(true)) {
                                    check_return!(self.interpret_stmts(vec![*then_stmt]));
                                } else if let Some(else_stmt) = else_stmt {
                                    check_return!(self.interpret_stmts(vec![*else_stmt]));
                                }
                            }
                            RuntimeEntity::ClassInstance(inst) => todo!("Condition must evaluate to a literal value. Instead got {}.", inst),
                            RuntimeEntity::Callable(f) => todo!("Condition must evaluate to a literal value. Insted got {}.", f)
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

    pub fn evaluate(&mut self, expr: Expr) -> Result<RuntimeEntity, LocErr> {
    	match expr.clone() {
            Expr::Set { object, ident, value } => {
                dbg!(&object, &ident, &value);
                println!("-------------------------------");
                
                let mut obj = match self.evaluate(*object)? {
                    RuntimeEntity::Literal(lit) => todo!("Expected the object to evaluate to a class instance, insted got {}.", lit),
                    RuntimeEntity::Callable(cal) => todo!("Expected the object to evaluate to a class instance, insted got {}.", cal),
                    RuntimeEntity::ClassInstance(inst) => inst,
                };

                let name = match ident.kind() {
                    TokenKind::Identifier(n) => n,
                    _ => unreachable!("Should be an identifier.")
                };

                let evald_val = self.evaluate(*value)?;

                obj.set(name.clone(), evald_val);
                // todo!("Should return the evaluated value.")
                Ok(RuntimeEntity::Literal(LitVal::Nil))
            }
            Expr::Get { object, ident } => {
                dbg!(&object, &ident);
                println!("-------------------------------");
                
                let fmtd_object = format!("{}", object);

                let mut obj = match self.evaluate(*object)? {
                    RuntimeEntity::Literal(lit) => todo!("Expected the object to evaluate to a class instance, insted got {}.", lit),
                    RuntimeEntity::Callable(cal) => todo!("Expected the object to evaluate to a class instance, insted got {}.", cal),
                    RuntimeEntity::ClassInstance(inst) => inst,
                };

                let name = match ident.kind() {
                    TokenKind::Identifier(n) => n,
                    _ => unreachable!("Should be an identifier.")
                };

                let property = match obj.get(name) {
                    Ok(prop) => prop,
                    Err(msg) => return Err(LocErr::new(ident.loc(), format!("Failed execution of `get` from {}.\n    [ERROR_from_get_property] {}", fmtd_object, msg)))
                };

                Ok(property)
            }
			Expr::Lambda { params, body, right_paren: _ } => {
				let lambda_indentifier = format!("Lambda_{:x}", rand::random::<usize>());
				let lambda_arity = params.len();
				
				// clone them to this scope for the closure to capture
				let params = params.clone();
				let body = body.clone();
                let global_env = self.globals.clone();
                let env = self.enviroment.clone();

				// define the lambda
				let lambda_impl = move |args: Vec<RuntimeEntity>| -> Result<Option<RuntimeEntity>, LocErr> {
					let mut lambda_interp = Interpreter::for_closure(global_env.clone(), env.clone());

					for (index, arg) in args.iter().enumerate() {
						let param_ident = match params[index].kind() {
							TokenKind::Identifier(ident) => ident.clone(),
							_ => unreachable!("Parameter in the `params` vector should be Identifier.")
						};
						lambda_interp.enviroment.borrow_mut()
                            .define(param_ident, arg.clone());
					}
					
					for i in 0..body.len() {
						check_return!(lambda_interp.interpret_stmts(vec![body[i].clone()]));
					}

					return Ok(Some(RuntimeEntity::Literal(LitVal::Nil)))
				};

				Ok(RuntimeEntity::Callable(
                    CallableEntity::Function(
                        Function::new(lambda_indentifier, lambda_arity, Rc::new(lambda_impl)))))
			}
			Expr::Call { callee, arguments, right_paren } => {
                // if the call fails, pass these to an error message
                let fmtd_args: Vec<_> = arguments.iter().map(|arg| format!("{}", arg)).collect();
                let fmtd_args = fmtd_args.join(", ");

				let callable = match self.evaluate(*callee)? {
                    RuntimeEntity::Callable(cl) => cl,
                    RuntimeEntity::Literal(lit) => todo!("Can't call an object that is doesn't implement Callable trait {}.", lit),
                    RuntimeEntity::ClassInstance(inst) => todo!("Can't call an object that is doesn't implement Callable trait {}.", inst),
                };

                let arguments_count = arguments.len();
                let mut evaled_args = vec![];
                for arg in arguments {
                    let evaled_arg = self.evaluate(arg)?;
                    evaled_args.push(evaled_arg);
                }  

				match &callable {
                    CallableEntity::Function(fun) => {
                        if fun.arity as usize != arguments_count {
							return Err(LocErr::new(right_paren.loc(), format!("Callable `{}` expects {} arguments, but received {}. Arity check failed.", &fun.ident, &fun.arity, arguments_count)))
						}

						let call_result = (fun.func)(evaled_args);
						match call_result {
							Ok(ret) => Ok(ret.expect("Every callable should at least return Nil. Every function returns nil implicitly.")),
							Err(err) => {
								Err(LocErr::new(
                                    right_paren.loc(), 
                                    format!("Failed execution of callable `{}` with ({}) as passed arguments.\n  [ERROR-from-callable][{}] {}", &fun.ident, fmtd_args, err.loc, err.msg)
                                ))
							}
						}
					}
                    CallableEntity::Class(cl) => {
                        if cl.arity() as usize != arguments_count {
							return Err(LocErr::new(
                                right_paren.loc(), 
                                format!("Callable `{}` expects {} arguments, but received {}. Arity check failed.", &cl.name, &cl.arity(), arguments_count)
                            ))
						}

						let call_result = cl.call(evaled_args);
						match call_result {
							Ok(ret) => Ok(ret.expect("Every callable should at least return Nil. Every function returns nil implicitly.")),
							Err(err) => {
								Err(LocErr::new(
                                    right_paren.loc(), 
                                    format!("Failed execution of callable `{}` with ({}) as passed arguments.\n  [ERROR-from-callable][{}] {}", &cl.name, fmtd_args, err.loc, err.msg)
                                ))
							}
						}
                    }
				}
			}
    	    Expr::Literal { value } => Ok(RuntimeEntity::new_from(value)),
    	    Expr::Grouping { expr } => self.evaluate(*expr),
    	    Expr::Unary { operator, expr } => {
        		let right = match self.evaluate(*expr)? {
                    RuntimeEntity::Literal(lit) => lit,
                    RuntimeEntity::Callable(cl) => return Err(LocErr::new(operator.loc(), format!("Can't apply unary operators to callable object {}.", cl))),
                    RuntimeEntity::ClassInstance(inst) => return Err(LocErr::new(operator.loc(), format!("Can't apply unary operators to callable object {}.", inst)))
                };

        		match (right, operator.kind()) {
        		    (LitVal::Int(x), TokenKind::Minus) => Ok(RuntimeEntity::new_from(LitVal::Int(-x))),
        		    (LitVal::Double(x), TokenKind::Minus) => Ok(RuntimeEntity::new_from(LitVal::Double(-x))),
        		    (any, TokenKind::Minus) => Err(LocErr::new(operator.loc(), format!("Minus operator not implemented for {:?}", any))),
        		    (any, TokenKind::Bang) => Ok(RuntimeEntity::new_from(any.is_falsy())),
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
                let left = match self.evaluate(*left.clone())? {
                    RuntimeEntity::Literal(lit) => lit,
                    RuntimeEntity::Callable(func) => return Err(LocErr::new(operator.loc(), format!("Left exression must evaluate to a literal value. Instead evaluated to {}.", func))),
                    RuntimeEntity::ClassInstance(inst) => return Err(LocErr::new(operator.loc(), format!("Left exression must evaluate to a literal value. Instead evaluated to {}.", inst))),
                };

                match operator.kind() {
					TokenKind::And => {
                        if left.is_truthy() == LitVal::Bool(false) {
                            return Ok(RuntimeEntity::new_from(left));
                        }
                    }
                    TokenKind::Or => {
                        if left.is_truthy() == LitVal::Bool(true) {
                            return Ok(RuntimeEntity::new_from(left))
                        }
                    }
                    _ => unreachable!("Can't perform a logical evaluation with non-logical operations. {:?}", operator)
                }

                Ok(self.evaluate(*right)?)
            }
    	    Expr::Binary { operator, left, right } => {
        		use LitVal::*;
                let left = match self.evaluate(*left)? {
                    RuntimeEntity::Literal(lit) => lit,
                    RuntimeEntity::Callable(func) => return Err(LocErr::new(operator.loc(), format!("Left exression must evaluate to a literal value. Instead evaluated to {}.", func))),
                    RuntimeEntity::ClassInstance(isnt) => return Err(LocErr::new(operator.loc(), format!("Left exression must evaluate to a literal value. Instead evaluated to {}.", isnt))),
                };

                let right = match self.evaluate(*right)? {
                    RuntimeEntity::Literal(lit) => lit,
                    RuntimeEntity::Callable(func) => return Err(LocErr::new(operator.loc(), format!("Right exression must evaluate to a literal value. Instead evaluated to {}.", func))),
                    RuntimeEntity::ClassInstance(inst) => return Err(LocErr::new(operator.loc(), format!("Right exression must evaluate to a literal value. Instead evaluated to {}.", inst))),
                };

        		match operator.kind() {
        		    TokenKind::Minus => {
            			match (left, right) {
            			    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Double(x - y))),
            			    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Int(x - y))),
            			    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Int(x as i32 - y))),
            			    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Int(x - y as i32))),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical values.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::Plus => {
            			match (left, right) {
            			    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Double(x + y))),
            			    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Int(x + y))),
            			    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Int(x as i32 + y))),
            			    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Int(x + y as i32))),
            			    (String(x), String(y))  => Ok(RuntimeEntity::new_from(String(format!("{}{}", x, y)))),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical or concatenable.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::Star => {
            			match (left, right) {
            			    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Double(x * y))),
            			    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Int(x * y))),
            			    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Double(x * f64::from(y)))),
            			    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Double(f64::from(x) * y))),
            			    (l, r) => {
								Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
							}
            			}
        		    }
        		    TokenKind::Slash => {
            			match (left, right) {
            			    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Double(x / y))),
            			    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Int(x / y))),
            			    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Double(x / f64::from(y)))),
            			    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Double(f64::from(x) / y))),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::Greater => {
            			match (left, right) {
            			    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Bool(x > y))),
            			    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Bool(x > y))),
            			    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Bool(x > f64::from(y)))),
            			    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Bool(f64::from(x) > y))),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::Less => {
            			match (left, right) {
            			    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Bool(x < y))),
            			    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Bool(x < y))),
            			    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Bool(x < f64::from(y)))),
            			    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Bool(f64::from(x) < y))),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::GreaterEqual => {
            			match (left, right) {
            			    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Bool(x >= y))),
            			    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Bool(x >= y))),
            			    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Bool(x >= f64::from(y)))),
            			    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Bool(f64::from(x) >= y))),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::LessEqual => {
            			match (left, right) {
            			    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Bool(x <= y))),
            			    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Bool(x <= y))),
            			    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Bool(x <= f64::from(y)))),
            			    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Bool(f64::from(x) <= y))),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::EqualEqual => {
            			match (left, right) {
            			    (Bool(x), Bool(y))      => Ok(RuntimeEntity::new_from(Bool(x == y))),
            			    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Bool(x == y))),
            			    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Bool(x == y))),
            			    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Bool(x == f64::from(y)))),
            			    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Bool(f64::from(x) == y))),
            			    (String(x), String(y))  => Ok(RuntimeEntity::new_from(Bool(x == y))),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical or boolean", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::BangEqual => {
            			match (left, right) {
            			    (Bool(x), Bool(y))      => Ok(RuntimeEntity::new_from(Bool(x != y))),
            			    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Bool(x != y))),
             			    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Bool(x != y))),
            			    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Bool(x != f64::from(y)))),
            			    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Bool(f64::from(x) != y))),
            			    (String(x), String(y))  => Ok(RuntimeEntity::new_from(Bool(x != y))),
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on non-numerical or non-boolean values {} and {}", operator.kind(), l, r)))
            			}
        		    }
        		    TokenKind::And | TokenKind::Or => unreachable!("Logical operations are not no be processed by the Binary branch."),
        		    e => Err(LocErr::new(operator.loc(), format!("Undefined binary operator {}.", e)))
        		}
    	    }
    	}
    }

    fn look_up_variable(&mut self, name: &String, expr: &Expr) -> Option<RuntimeEntity> {
        let distance = self.locals.get(expr);
        match distance {
            Some(dist) => self.enviroment.as_ref().borrow().get_at(dist, name),
            None => self.globals.as_ref().borrow().get(name)
        }
    }
}
