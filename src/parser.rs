
#[derive(Debug, Clone)]
pub enum LitVal { Int(i32), Double(f64), String(String), Bool(bool), Nil }

#[derive(Debug)]
pub enum Expr {
    Literal  { value: LitVal },
    Grouping { expr: Box<Expr> },
    Unary    { operator: Token, expr: Box<Expr> },
    Binary   { operator: Token, left: Box<Expr>, right: Box<Expr> }
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

    pub fn evaluate(&self) -> Result<LitVal, String> {
	match &self {
	    Expr::Literal { value } => Ok(value.clone()),
	    Expr::Grouping { expr } => (*expr).evaluate(),
	    Expr::Unary { operator, expr } => {
		let right = expr.evaluate()?;
		match (right, operator.kind()) {
		    (LitVal::Int(x), TokenKind::Minus) => Ok(LitVal::Int(-x)),
		    (LitVal::Double(x), TokenKind::Minus) => Ok(LitVal::Double(-x)),
		    (any, TokenKind::Minus) => Err(format!("Minus operator not implemented for {:?}", any)),
		    (any, TokenKind::Bang) => Ok(any.is_falsy()),
		    _ => todo!()
		}
	    }
	    Expr::Binary { operator, left, right } => {
		use LitVal::*;
		let left = left.evaluate()?;
		let right = right.evaluate()?;
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
            LitVal::Int(v)    => write!(f, "Int({})", v),
            LitVal::Double(v) => write!(f, "Double({})", v),
            LitVal::String(v) => write!(f, "String(\"{}\")", v),
            LitVal::Bool(v)   => write!(f, "Bool({})", if *v {"true"} else {"false"}),
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

impl Expr {
    pub fn print(&self) {
        println!("{}", self);
    }
}


use crate::token::{Token, TokenKind};
pub struct Parser {
    tokens: Vec<Token>,
    current: i32,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            current: 0,
        }
    }
    
    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<Expr, String> {
	self.tokens = tokens;
        self.expression()
    }

    // helpers --------------------------------------
    fn is_at_end(&self) -> bool {
        self.current as usize >= self.tokens.len() || matches!(self.peek().kind(), TokenKind::Eof) 
    }
    
    fn peek(&self) -> &Token {
        self.tokens.get(self.current as usize).unwrap()
    }

    fn previous(&self) -> Result<&Token, String> {
        if self.is_at_end() || (self.current - 1) as usize >= self.tokens.len() {
            Err(format!("Couldn't read previous token of {:?}!", self.peek()))
        } else {
            Ok(self.tokens.get((self.current - 1) as usize).unwrap())
        }
    }

    fn advance(&mut self) -> &Token {
        let t = self.tokens.get(self.current as usize).unwrap();
        if !self.is_at_end() {
            self.current += 1;
        }
        t
    }

    fn consume(&mut self, tok_kind: TokenKind, msg: String) -> Result<&Token, String> { 
        if *self.peek().kind() == tok_kind {
            Ok(self.advance())
        } else {
            Err(msg)
        }
    }
    //Error handling
    fn synchronize(&mut self) {
        use TokenKind::*;
        while !self.is_at_end() {
            if matches!(self.previous().unwrap().kind(), Semicolon) || matches!(self.peek().kind(), Fun | Class | Var | For | If | While | Print | Return) {
                return;
            }
            self.advance();
        }
    }

    //AST Syntax ------------------------------------
    fn expression(&mut self) -> Result<Expr, String> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.comparison()?;
        use TokenKind::*;
        while matches!(self.peek().kind(), BangEqual | EqualEqual) {
            self.advance();
            let op = self.previous()?.clone();
            let right = self.comparison()?;
            expr = Expr::new_binary(expr, op, right);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.term()?;
        use TokenKind::*;
        while matches!(self.peek().kind(), Greater | GreaterEqual | Less | LessEqual) {
            self.advance();
            let op = self.previous()?.clone();
            let right = self.term()?;
            expr = Expr::new_binary(expr, op, right);
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, String> {
        let mut expr = self.factor()?;
        use TokenKind::*;
        while matches!(self.peek().kind(), Minus | Plus) {
            self.advance();
            let op = self.previous()?.clone();
            let right = self.factor()?;
            expr = Expr::new_binary(expr, op, right);
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;
        use TokenKind::*;
        while matches!(self.peek().kind(), Star | Slash) {
            self.advance();
            let op = self.previous()?.clone();
            let right = self.unary()?;
            expr = Expr::new_binary(expr, op, right);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        use TokenKind::*;
        if matches!(self.peek().kind(), Bang | Minus) {
            self.advance();
            let op = self.previous()?.clone();
            let expr = self.unary()?;
            return Ok(Expr::new_unary(op, expr))
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, String> {
        match self.peek().kind() {
            TokenKind::False => {
                self.advance();
                Ok(Expr::new_literal(LitVal::Bool(false)))
            }
            TokenKind::True => {
                self.advance();
                Ok(Expr::new_literal(LitVal::Bool(true)))
            }
            TokenKind::Nil => {
                self.advance();
                Ok(Expr::new_literal(LitVal::Nil))
            }
            TokenKind::Int(v) => {
                let v = *v;
                self.advance();
                Ok(Expr::new_literal(LitVal::Int(v)))
            }
            TokenKind::Double(v) => {
                let v = *v;
                self.advance();
                Ok(Expr::new_literal(LitVal::Double(v)))
            }
            TokenKind::String(v) => {
                let v = v.clone();
                self.advance();
                Ok(Expr::new_literal(LitVal::String(v)))
            }
            TokenKind::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenKind::RightParen, "Expected ')' after expression".to_string())?;
                Ok(Expr::new_grouping(expr))
            }
            TokenKind::Eof => {
                println!("\nBye, bye :(");
                std::process::exit(0);
            }
            _ => Err(
                format!("[ERROR]: {} Expected Expression after {:?} operator, but found {:?}!", 
                self.peek().loc(), self.previous()?.kind(), self.peek().kind())
            )
        }
    }
}
