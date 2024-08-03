use crate::{enviroment::Enviroment, token::{Token, TokenKind}};

#[derive(Debug, Clone)]
pub enum LitVal { Int(i32), Double(f64), String(String), Bool(bool), Nil }

#[derive(Debug, Clone)]
pub enum Expr {
    Literal  { value: LitVal },
    Grouping { expr: Box<Expr> },
    Unary    { operator: Token, expr: Box<Expr> },
    Binary   { operator: Token, left: Box<Expr>, right: Box<Expr> },
    Var      { ident: Token },
    Assign   { target: Token, value: Box<Expr> },
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
    
    pub fn evaluate(&self, env: &mut Enviroment) -> Result<LitVal, String> {
    	match self {
    	    Expr::Literal { value } => Ok(value.clone()),
    	    Expr::Grouping { expr } => (*expr).evaluate(env),
    	    Expr::Unary { operator, expr } => {
        		let right = expr.evaluate(env)?;
        		match (right, operator.kind()) {
        		    (LitVal::Int(x), TokenKind::Minus) => Ok(LitVal::Int(-x)),
        		    (LitVal::Double(x), TokenKind::Minus) => Ok(LitVal::Double(-x)),
        		    (any, TokenKind::Minus) => Err(format!("Minus operator not implemented for {:?}", any)),
        		    (any, TokenKind::Bang) => Ok(any.is_falsy()),
        		    _ => todo!()
        		}
    	    }
            Expr::Assign { target, value } => {
                if let TokenKind::Identifier(i) = target.kind() {
                    let v = value.evaluate(env)?;
                    return match env.assign(i.to_string(), v.clone()) {
                        Some(_) => Ok(v),
                        None => Err(format!("{} Can't assign {} to an undeclared target {}.", target.loc(), v, i))
                    }
                }
                Err(format!(""))
            }
            Expr::Var { ident } => {
				if let TokenKind::Identifier(i) = ident.kind() {
					match env.get(i.clone()) {
						Some(value) => return Ok(value.clone()),
						None => return Err(format!("{} Variable {} is undeclared.", ident.loc(), i))
					}
				}
				panic!("{} Totally fucked up. {:?} is not an identifier.", ident.loc(), ident.kind());
            }
    	    Expr::Binary { operator, left, right } => {
        		use LitVal::*;
        		let left = left.evaluate(env)?;
        		let right = right.evaluate(env)?;
        		let op = operator.kind();
        		match op {
        		    TokenKind::Minus => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Double(x - y)),
            			    (Int(x), Int(y)) => Ok(Int(x - y)),
            			    (Double(x), Int(y)) => Ok(Int(x as i32 - y)),
            			    (Int(x), Double(y)) => Ok(Int(x - y as i32)),
            			    (l, r) => Err(format!("Can't perform {:?} operation on {} and {}. Operands must be numerical values.", op, l, r))
            			}
        		    }
        		    TokenKind::Plus => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Double(x + y)),
            			    (Int(x), Int(y)) => Ok(Int(x + y)),
            			    (Double(x), Int(y)) => Ok(Int(x as i32 + y)),
            			    (Int(x), Double(y)) => Ok(Int(x + y as i32)),
            			    (String(x), String(y)) => Ok(String(format!("{}{}", x, y))),
            			    (l, r) => Err(format!("Can't perform {:?} operation on {} and {}. Operands must be numerical or concatenable.", op, l, r))
            			}
        		    }
        		    TokenKind::Star => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Double(x * y)),
            			    (Int(x), Int(y)) => Ok(Int(x * y)),
            			    (Double(x), Int(y)) => Ok(Double(x * f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Double(f64::from(x) * y)),
            			    (l, r) => Err(format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", op, l, r))
            			}
        		    }
        		    TokenKind::Slash => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Double(x / y)),
            			    (Int(x), Int(y)) => Ok(Int(x / y)),
            			    (Double(x), Int(y)) => Ok(Double(x / f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Double(f64::from(x) / y)),
            			    (l, r) => Err(format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", op, l, r))
            			}
        		    }
        		    TokenKind::Greater => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Bool(x > y)),
            			    (Int(x), Int(y)) => Ok(Bool(x > y)),
            			    (Double(x), Int(y)) => Ok(Bool(x > f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Bool(f64::from(x) > y)),
            			    (l, r) => Err(format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", op, l, r))
            			}
        		    }
        		    TokenKind::Less => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Bool(x < y)),
            			    (Int(x), Int(y)) => Ok(Bool(x < y)),
            			    (Double(x), Int(y)) => Ok(Bool(x < f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Bool(f64::from(x) < y)),
            			    (l, r) => Err(format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", op, l, r))
            			}
        		    }
        		    TokenKind::GreaterEqual => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Bool(x >= y)),
            			    (Int(x), Int(y)) => Ok(Bool(x >= y)),
            			    (Double(x), Int(y)) => Ok(Bool(x >= f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Bool(f64::from(x) >= y)),
            			    (l, r) => Err(format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", op, l, r))
            			}
        		    }
        		    TokenKind::LessEqual => {
            			match (left, right) {
            			    (Double(x), Double(y)) => Ok(Bool(x <= y)),
            			    (Int(x), Int(y)) => Ok(Bool(x <= y)),
            			    (Double(x), Int(y)) => Ok(Bool(x <= f64::from(y))),
            			    (Int(x), Double(y)) => Ok(Bool(f64::from(x) <= y)),
            			    (l, r) => Err(format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", op, l, r))
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
            			    (l, r) => Err(format!("Can't perform {:?} operation on {} and {}. Operands must be numerical or boolean", op, l, r))
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
            			    (l, r) => Err(format!("Can't perform {:?} operation on non-numerical or non-boolean values {} and {}", op, l, r))
            			}
        		    }
        		    TokenKind::And => {
            			match (left, right) {
            			    (Bool(x), Bool(y)) => Ok(Bool(x && y)),
            			    (l, r) => Err(format!("Cannot perform {:?} operation on {} and {}. Operands must be boolean.", op, l, r))
            			}
        		    }
        		    TokenKind::Or => {
            			match (left, right) {
            			    (Bool(x), Bool(y )) => Ok(Bool(x || y)),
            			    (l, r) => Err(format!("Cannot perform {:?} operation on {} and {}. Operands msut be boolean.", op, l, r))
            			}
        		    }
        		    e => Err(format!("Unknown operator {:?}", e))
        		}
    	    }
    	}
    }

}

impl LitVal {
    fn is_falsy(&self) -> LitVal {
    	match self {
    	    LitVal::Int(x) => LitVal::Bool(*x == 0 as i32),
    	    LitVal::Double(x) => LitVal::Bool(*x == 0.0 as f64),
    	    LitVal::String(x) => LitVal::Bool(x.is_empty()),
    	    LitVal::Bool(x) => LitVal::Bool(!x),
    	    LitVal::Nil => LitVal::Bool(true),
    	}
    }
}

impl std::fmt::Display for LitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    	match self {
            LitVal::Int(v)    => write!(f, "int({})", v),
            LitVal::Double(v) => write!(f, "double({})", v),
            LitVal::String(v) => write!(f, "string(\"{}\")", v),
            LitVal::Bool(v)   => write!(f, "bool({})", if *v {"true"} else {"false"}),
            LitVal::Nil       => write!(f, "nil"),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal { value } => {
        		write!(f, "{}", value)
            }
            Self::Grouping { expr } => {
                write!(f, "{}", expr)
            }
            Self::Unary { operator, expr } => {
                match operator.kind() {
                    TokenKind::Minus => write!(f, "(- {})", expr),
                    TokenKind::Bang  => write!(f, "(! {})", expr),
                    err => todo!("Unary operator {:?} not supported", err),
                }
            }
            Self::Var { ident: _ } => {
                Ok(())
            }
            Self::Assign { target: _, value: _ } => {
                Ok(())
            }
            Self::Binary { operator, left, right } => {
                match operator.kind() {
                    TokenKind::Plus         => write!(f, "(+ {} {})", left, right),
                    TokenKind::Minus        => write!(f, "(- {} {})", left, right),
                    TokenKind::Star         => write!(f, "(* {} {})", left, right),
                    TokenKind::Slash        => write!(f, "(/ {} {})", left, right),
                    TokenKind::EqualEqual   => write!(f, "(== {} {})", left, right),
                    TokenKind::BangEqual    => write!(f, "(!= {} {})", left, right),
                    TokenKind::Less         => write!(f, "(< {} {})", left, right),
                    TokenKind::LessEqual    => write!(f, "(<= {} {})", left, right),
                    TokenKind::Greater      => write!(f, "(> {} {})", left, right),
                    TokenKind::GreaterEqual => write!(f, "(>= {} {})", left, right),
                    TokenKind::And          => write!(f, "(and {} {})", left, right),
                    TokenKind::Or           => write!(f, "(or {} {})", left, right),
                    err => todo!("Binary operator {:?} not supported", err),
                }
            }
        }
    }
}
