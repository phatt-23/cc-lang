use std::{cell::RefCell, rc::Rc};
use crate::{enviroment::Environment, interpreter::Interpreter, literal_value::LitVal, loc_error::LocErr, statement::Stmt, token::{Token, TokenKind}};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal  { value: LitVal },
    Grouping { expr: Box<Expr> },
    Unary    { operator: Token, expr: Box<Expr> },
    Binary   { operator: Token, left: Box<Expr>, right: Box<Expr> },
    Var      { ident: Token },
    Assign   { target: Token, value: Box<Expr> },
    Logical  { operator: Token, left: Box<Expr>, right: Box<Expr> },
	Call     { callee: Box<Expr>, arguments: Vec<Expr>, right_paren: Token },
	Lambda   { params: Vec<Token>, body: Vec<Stmt>, right_paren: Token }
}

impl Expr {
	// Constructors
    pub fn new_binary(left: Expr , operator: Token, right: Expr) -> Self {
        Self::Binary { operator, left: Box::from(left), right: Box::from(right) }
    }

    pub fn new_unary(operator: Token, expr: Expr) -> Self {
        Self::Unary { operator, expr: Box::from(expr) }
    }

    pub fn new_grouping(expr: Expr) -> Self {
        Self::Grouping { expr: Box::from(expr) }
    }

    pub fn new_literal(value: LitVal) -> Self {
        Self::Literal { value }
    }

    pub fn new_assign(target: Token, value: Expr) -> Self {
        Self::Assign { target, value: Box::from(value) }
    }
    
    pub fn new_logical(left: Expr, operator: Token, right: Expr) -> Self {
        Self::Logical { operator, left: Box::from(left), right: Box::from(right) }
    }

	pub fn new_call(callee: Box<Expr>, arguments: Vec<Expr>, right_paren: Token) -> Self{
		Self::Call { callee, arguments, right_paren }
	}

	pub fn new_lambda(params: Vec<Token>, body: Vec<Stmt>, right_paren: Token) -> Self {
		Self::Lambda { params, body, right_paren } 
	}
}

impl Expr {
    pub fn evaluate(&self, env: Rc<RefCell<Environment>>) -> Result<LitVal, LocErr> {
    	match self {
			Expr::Lambda { params, body, right_paren: _ } => {
				println!("process lambda: {}", self);
				let lambda_indentifier = format!("Lambda_{:x}", rand::random::<usize>());
				let lambda_arity = params.len();
				
				// clone them to this scope for the closure to capture
				let params = params.clone();
				let body = body.clone();
				// define the lambda
				let lambda_impl = move |args: Vec<LitVal>| -> Result<LitVal, LocErr> {
					let mut lambda_interp = Interpreter::for_closure(env.clone());

					for (index, arg) in args.iter().enumerate() {
						let param_ident = match params[index].kind() {
							TokenKind::Identifier(ident) => ident.clone(),
							_ => unreachable!("Parameter in the `params` vector should be Identifier.")
						};
						lambda_interp.enviroment.borrow_mut().define(param_ident, arg.clone());
					}
					
					for i in 0..body.len() {
						match lambda_interp.interpret(vec![body[i].clone()]) {
							Ok(opt_ret) => match opt_ret {
								Some(ret) => return Ok(ret.value),
								None => return Ok(LitVal::Nil),
							}
							Err(err) => return Err(err)
						}
					}

					return Ok(LitVal::Nil)
				};

				Ok(LitVal::Callable { 
					ident: lambda_indentifier, 
					arity: lambda_arity, 
					func: Rc::new(lambda_impl)
				})
			}
			Expr::Call { callee, arguments, right_paren } => {
				// println!("Calling: {}", self);
				let callable = callee.evaluate(env.clone())?;

				match &callable {
					LitVal::Callable { ident, arity, ref func } => {
						if *arity as usize != arguments.len() {
							return Err(LocErr::new(right_paren.loc(), format!("Callable `{}` expects {} arguments, but received {}. Arity check failed.", &ident, &arity, arguments.len())))
						}

						let mut evaled_args = vec![];
						for arg in arguments {
							let evaled_arg = arg.evaluate(env.clone())?;
							evaled_args.push(evaled_arg);
						}  

						let call_result = func(evaled_args);
						match call_result {
							Ok(ok) => Ok(ok),
							Err(err) => {
								let fmtd_args: Vec<_> = arguments.iter().map(|arg| format!("{}", arg)).collect();
								let fmtd_args = fmtd_args.join(", ");
								
								Err(LocErr::new(right_paren.loc(), format!("Failed execution of callable `{}` with ({}) as passed arguments.\n  [ERROR-from-callable][{}] {}", &ident, fmtd_args, err.loc, err.msg)))
							}
						}
					}
					any => Err(LocErr::new(right_paren.loc(), format!("Trying to call `{}` of value {}, which is not callable.", callee, any)))
				}
			}
    	    Expr::Literal { value } => Ok(value.clone()),
    	    Expr::Grouping { expr } => (*expr).evaluate(env),
    	    Expr::Unary { operator, expr } => {
        		let right = expr.evaluate(env)?;
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
                    let v = value.evaluate(env.clone())?;
                    return match env.as_ref().borrow_mut().assign(i, v.clone()) {
                        Some(_) => Ok(v),
                        None => Err(LocErr::new(target.loc(), format!("Can't assign {} to an undeclared target `{}`.", v, i)))
                    }
                }
                unreachable!("{} Target must be an identifier", target.loc());
            }
            Expr::Var { ident } => {
				if let TokenKind::Identifier(i) = ident.kind() {
					match env.as_ref().borrow().get(i) {
						Some(value) => return Ok(value.clone()),
						None => return Err(LocErr::new(ident.loc(), format!("Variable with identifier `{}` is undeclared.", i)))
					}
				}
				unreachable!("{} Totally fucked up. {:?} is not an identifier.", ident.loc(), ident.kind());
            }
            Expr::Logical { operator, left, right } => {
                let left = left.evaluate(env.clone())?;
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
                    _ => unreachable!("Can't perform a logical evaluation with non-logical operations. {:?}", &self)
                }

                Ok(right.evaluate(env)?)
            }
    	    Expr::Binary { operator, left, right } => {
        		use LitVal::*;
        		let left = left.evaluate(env.clone())?;
        		let right = right.evaluate(env)?;

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

}

impl LitVal {
    pub fn is_falsy(&self) -> LitVal {
    	match self {
    	    LitVal::Int(x) => LitVal::Bool(*x == 0_i32),
    	    LitVal::Double(x) => LitVal::Bool(*x == 0.0_f64),
    	    LitVal::String(x) => LitVal::Bool(x.is_empty()),
    	    LitVal::Bool(x) => LitVal::Bool(!x),
    	    LitVal::Nil => LitVal::Bool(true),
			LitVal::Callable { ident, arity, func: _ } => todo!("Can you even call this on a callable? {} {}", ident, arity),
    	}
    }

    pub fn is_truthy(&self) -> LitVal {
		match self {
			LitVal::Int(x) 		=> LitVal::Bool(*x != 0_i32),
    	    LitVal::Double(x) 	=> LitVal::Bool(*x != 0.0_f64),
    	    LitVal::String(x) 	=> LitVal::Bool(!x.is_empty()),
    	    LitVal::Bool(x) 	=> LitVal::Bool(*x),
    	    LitVal::Nil 		=> LitVal::Bool(false),
			LitVal::Callable { ident, arity, func: _ } => todo!("Can you even call this on a callable? {} {}", ident, arity),
    	}
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
			Self::Lambda { params, body, right_paren: _} => {
				let formatted_params: Vec<String> = 
					params
						.iter()
						.map(|param| format!("{}", param.kind()))
						.collect();

				let formatted_body: Vec<String> = 
					body
						.iter()
						.map(|b| format!("{}", b))
						.collect();
				
				let mut fmtd_string = format!("(:lambda ({}) ", formatted_params.join(" "));
				if body.len() == 1 {
					fmtd_string.push_str(format!("{})", formatted_body.join(" ")).as_str());
				} else {
					fmtd_string.push_str(format!("({}))", formatted_body.join(" ")).as_str());
				}
				
				write!(f, "{}", fmtd_string)				
			}
			Self::Call { callee, arguments, right_paren: _ } => {
				let formatted_args: Vec<String> = 
					arguments
						.iter()
						.map(|arg| format!("{}", arg))
						.collect();

				write!(f, "({} ({}))", callee, formatted_args.join(" "))
			}
            Self::Literal { value } => {
				write!(f, "{}", value)
			}
            Self::Grouping { expr } => {
                write!(f, "({})", expr)
            }
            Self::Unary { operator, expr } => {
				write!(f, "({} {})", operator.kind(), expr)
            }
            Self::Var { ident } => {
                write!(f, "(var {})", ident.kind())
            }
            Self::Assign { target, value } => {
                write!(f, "(setq {} {})", target.kind(), value)
            }
            Self::Logical { operator, left, right } => {
				write!(f, "({} {} {})", operator.kind(), left, right)
            }
            Self::Binary { operator, left, right } => {
				write!(f, "({} {} {})", operator.kind(), left, right)
            }
        }
    }
}
