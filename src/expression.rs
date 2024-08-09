use std::hash::{Hash, Hasher};
use crate::{literal_value::LitVal, statement::Stmt, token::Token};

#[derive(Debug, Clone, PartialEq, Eq)]
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

impl Hash for Expr {
	fn hash<H: Hasher>(&self, state: &mut H) {
		match self {
			Self::Assign { target, value } => {
				0_u8.hash(state);
				target.hash(state);
				value.hash(state);
			}
			Self::Binary { operator, left, right } => {
				1_u8.hash(state);
				operator.hash(state);
				left.hash(state);
				right.hash(state);
			}
			Self::Call { callee, arguments, right_paren: _ } => {
				2_u8.hash(state);
				callee.hash(state);
				arguments.hash(state);
				// right_paren.hash(state);
			}
			Self::Grouping { expr } => {
				3_u8.hash(state);
				expr.hash(state);
			}
			Self::Lambda { params, body, right_paren: _ } => {
				4_u8.hash(state);
				params.hash(state);
				body.hash(state);
				// right_paren.hash(state);
			}
			Self::Literal { value } => {
				5_u8.hash(state);
				value.hash(state);
			}
			Self::Logical { operator, left, right } => {
				6_u8.hash(state);
				operator.hash(state);
				left.hash(state);
				right.hash(state);
			}
			Self::Unary { operator, expr } => {
				7_u8.hash(state);
				operator.hash(state);
				expr.hash(state);
			}
			Self::Var { ident } => {
				8_u8.hash(state);
				ident.hash(state);
			}
		}
	}
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


#[cfg(test)]
mod tests {
	use crate::{parser::Parser, lexer::Lexer, statement::Stmt};
	use std::hash::{DefaultHasher, Hash, Hasher};

    #[test]
    fn expr_is_hashable() {
		let mut lexer = Lexer::new();
		let mut parser = Parser::new();

		let tokens = lexer.lex("nothing".to_string(), "12 + 45 * 90; 12 + 45 * 90;".to_string()).unwrap();
		let stmts = parser.parse(tokens).unwrap();

		let e0 = match stmts[0].clone() {
			Stmt::Expression { expression } => expression,
			_ => todo!()
		};

		let e1 = match stmts[1].clone() {
			Stmt::Expression { expression } => expression,
			_ => todo!()
		};

		let mut hasher = DefaultHasher::new();
		e0.hash(&mut hasher);
		let e0_hash = hasher.finish();
		println!("{} :::: {}", e0, e0_hash);
		
		let mut hasher = DefaultHasher::new();
		e1.hash(&mut hasher);
		let e1_hash = hasher.finish();
		println!("{} :::: {}", e1, e1_hash);
		
		let s0 = stmts[0].clone();
		let mut hasher = DefaultHasher::new();
		s0.hash(&mut hasher);
		let s0_hash = hasher.finish();
		println!("{} :::: {}", s0, s0_hash);
		
		let s1 = stmts[1].clone();
		let mut hasher = DefaultHasher::new();
		s1.hash(&mut hasher);
		let s1_hash = hasher.finish();
		println!("{} :::: {}", s1, s1_hash);
		

		assert_eq!(e0_hash, e1_hash);
		assert_eq!(s0_hash, s1_hash);
    }
}