#[derive(Debug)]
pub enum LitVal { Int(i32), Double(f64), String(String), True, False, Nil }

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
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal { value } => {
                match value {
                    LitVal::Int(v)    => write!(f, "{}", v),
                    LitVal::Double(v) => write!(f, "{}", v),
                    LitVal::String(v) => write!(f, "{}", v),
                    LitVal::False     => write!(f, "false"),
                    LitVal::True      => write!(f, "true"),
                    LitVal::Nil       => write!(f, "nil"),
                }
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
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
        }
    }
    
    pub fn parse(&mut self) -> Expr {
        self.expression()
    }

    // helpers --------------------------------------
    fn is_at_end(&self) -> bool {
        // matches!(self.peek().kind(), TokenKind::Eof)
        self.current >= self.tokens.len()
    }
    
    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn consume(&mut self, _tok_kind: TokenKind, msg: String) -> &Token {
        if matches!(self.peek().kind(), _tok_kind) {
            return self.advance();
        } 
        panic!("{}", msg);
    }

    //AST Syntax ------------------------------------
    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        use TokenKind::*;
        while matches!(self.peek().kind(), BangEqual | EqualEqual) {
            self.advance();
            let op = self.previous().clone();
            let right = self.comparison();
            expr = Expr::new_binary(expr, op, right);
        }
        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();
        use TokenKind::*;
        while matches!(self.peek().kind(), Greater | GreaterEqual | Less | LessEqual) {
            self.advance();
            let op = self.previous().clone();
            let right = self.term();
            expr = Expr::new_binary(expr, op, right);
        }
        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        use TokenKind::*;
        while matches!(self.peek().kind(), Minus | Plus) {
            self.advance();
            let op = self.previous().clone();
            let right = self.factor();
            expr = Expr::new_binary(expr, op, right);
        }
        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();
        use TokenKind::*;
        while matches!(self.peek().kind(), Star | Slash) {
            self.advance();
            let op = self.previous().clone();
            let right = self.unary();
            expr = Expr::new_binary(expr, op, right);
        }
        expr
    }

    fn unary(&mut self) -> Expr {
        use TokenKind::*;
        if matches!(self.peek().kind(), Bang | Minus) {
            self.advance();
            let op = self.previous().clone();
            let expr = self.unary();
            return Expr::new_unary(op, expr);
        }
        self.primary()
    }

    fn primary(&mut self) -> Expr {
        match self.peek().kind() {
            TokenKind::False => {
                self.advance();
                Expr::new_literal(LitVal::False)
            }
            TokenKind::True => {
                self.advance();
                Expr::new_literal(LitVal::True)
            }
            TokenKind::Nil  => {
                self.advance();
                Expr::new_literal(LitVal::Nil)
            }
            TokenKind::Int(v) => {
                let v = *v;
                self.advance();
                Expr::new_literal(LitVal::Int(v))
            }
            TokenKind::Double(v) => {
                let v = *v;
                self.advance();
                Expr::new_literal(LitVal::Double(v))
            }
            TokenKind::String(v) => {
                let v = v.clone();
                self.advance();
                Expr::new_literal(LitVal::String(v))
            }
            TokenKind::LeftParen => {
                self.advance();
                let expr = self.expression();
                self.consume(TokenKind::RightParen, "Expected ')' after expression".to_string());
                Expr::new_grouping(expr)
            }
            _ => panic!("primary not resolved"),
        }
    }
}