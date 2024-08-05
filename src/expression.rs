use crate::{enviroment::Enviroment, literal_value::LitVal, loc_error::LocErr, token::{Token, TokenKind}};

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
}

impl Expr {
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
}

impl Expr {
    pub fn evaluate(&self, env: &mut Enviroment) -> Result<LitVal, LocErr> {
    	match self {
			Expr::Call { callee, arguments, right_paren } => {
				println!("Calling: {}", self);
				let callable = callee.evaluate(env)?;

				match &callable {
					LitVal::Callable { ident, arity, func } => {
						if *arity as usize != arguments.len() {
							return Err(LocErr::new(right_paren.loc(), format!("Callable `{}` expects {} arguments, but got {}. Arity check failed.", &ident, &arity, arguments.len())))
						}

						let mut evaled_args = vec![];
						for arg in arguments {
							let evaled_arg = arg.evaluate(env)?;
							evaled_args.push(evaled_arg);
						}  

						Ok(func(evaled_args))
					}
					any => Err(LocErr::new(right_paren.loc(), format!("Trying to call {} of value {}, which is not callable.", callee, any)))
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
                    let v = value.evaluate(env)?;
                    return match env.assign(i, v.clone()) {
                        Some(_) => Ok(v),
                        None => Err(LocErr::new(target.loc(), format!("Can't assign {} to an undeclared target {}.", v, i)))
                    }
                }
                // TODO: Try with "hello" = 12;
                unreachable!("{} Target must be an identifier", target.loc());
            }
            Expr::Var { ident } => {
				if let TokenKind::Identifier(i) = ident.kind() {
					match env.get(i) {
						Some(value) => return Ok(value.clone()),
						None => return Err(LocErr::new(ident.loc(), format!("Variable {} is undeclared.", i)))
					}
				}
                // todo: Try with var "hello" = 23;
				unreachable!("{} Totally fucked up. {:?} is not an identifier.", ident.loc(), ident.kind());
            }
            Expr::Logical { operator, left, right } => {
                let left = left.evaluate(env)?;
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
        		let left = left.evaluate(env)?;
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
            			    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
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

impl std::fmt::Display for LitVal {
    // fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    // 	match self {
    //         LitVal::Int(v)    => write!(f, "int({})", v),
    //         LitVal::Double(v) => write!(f, "double({})", v),
    //         LitVal::String(v) => write!(f, "string(\"{}\")", v),
    //         LitVal::Bool(v)   => write!(f, "bool({})", if *v {"true"} else {"false"}),
    //         LitVal::Nil       => write!(f, "nil"),
    //     }
    // }

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LitVal::Int(v)    => write!(f, "{}", v),
			LitVal::Double(v) => write!(f, "{}", v),
			LitVal::String(v) => write!(f, "\"{}\"", v),
			LitVal::Bool(v)   => write!(f, "{}", v),
			LitVal::Nil       => write!(f, "nil"),
			LitVal::Callable { ident, arity, func: _ } => write!(f, "{}/{}", ident, arity),
        }
    }
}


impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
