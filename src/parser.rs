use crate::token::{Token, TokenKind};
use crate::statement::Stmt;
use crate::expression::{Expr, LitVal};

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
    
    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<Vec<Stmt>, String> {
	    self.tokens = tokens;

	    let mut stmts: Vec<Stmt> = Vec::new();
	    let mut errs: Vec<String> = Vec::new();

	    while !self.is_at_end() {
	        let stmt = self.declaration();
	        match stmt {
    	    	Ok(s) => stmts.push(s),
    	    	Err(msg) => {
    	    	    errs.push(msg);
    	    	    self.synchronize();
	            }
	        }
	    }

	    if errs.is_empty() {
	        return Ok(stmts)
	    }
        Err(errs.join("\n"))
    }

    fn declaration(&mut self) -> Result<Stmt, String> {
        let tok = self.peek();
        if matches!(tok.kind(), TokenKind::Var) {
            let var_decl = self.var_declaration();
            match var_decl {
                Ok(stmt) => return Ok(stmt),
                Err(msg) => {
                    self.synchronize();
                    return Err(msg);
                }
            }
        }
        self.statement()
    }
    
    fn var_declaration(&mut self) -> Result<Stmt, String> {
        self.advance();
        let ident = self.advance().clone();
        match ident.kind() {
            TokenKind::Identifier(_name) => {
                let expression = if matches!(self.peek().kind(), TokenKind::Equal) {
                                    self.advance();
                                    self.expression()?
                                } else {
                                    Expr::Literal { value: LitVal::Nil }
                                };

                self.consume(
                    TokenKind::Semicolon, 
                    format!("{} Expected semicolon ';' denoting end of variable declaration. Found {:?}.", &self.peek().loc(), self.peek().kind())
                )?;
                Ok(Stmt::Var { ident, expression })
            }
            any => Err(format!("{} Expected Identifier after `var` keyword, but found {:?}.", self.peek().loc(), any))
        }
    }
    
    fn statement(&mut self) -> Result<Stmt, String> {
        let tok = self.peek();
	    match tok.kind() {
	        TokenKind::Print => self.print_statement(),
	        _ => self.expression_statement(),
	    }
    }

    fn print_statement(&mut self) -> Result<Stmt, String> {
        self.advance();
        let expr = self.expression()?;
        self.consume(TokenKind::Semicolon, format!("{} Expected a semicolon ';' after expression denoting end of statement.", self.peek().loc()))?;
        Ok(Stmt::Print {
            expression: expr
        })
    }

    fn expression_statement(&mut self) -> Result<Stmt, String> {
        let expr = self.expression()?;
        self.consume(TokenKind::Semicolon, format!("{} Expected a semicolon ';' after expression denoting end of statement.", self.peek().loc()))?;
        Ok(Stmt::Expression {
            expression: expr
        })
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
            Err(format!("{} Couldn't read previous token of {:?}!", self.peek().loc(), self.peek().kind()))
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

    fn synchronize(&mut self) {
        use TokenKind::*;
        self.advance();
        while !self.is_at_end() {
            if matches!(self.previous().unwrap().kind(), Semicolon) || matches!(self.peek().kind(), Fun | Class | Var | For | If | While | Print | Return) {
                return;
            }
            self.advance();
        }
    }

    //AST Syntax ------------------------------------
    fn expression(&mut self) -> Result<Expr, String> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, String> {
        let expr = self.equality()?;

        if matches!(self.peek().kind(), TokenKind::Equal) {
            self.advance();
            let value = self.assignment()?;
            if let Expr::Var {ident} = expr {
                return Ok(Expr::new_assign(ident, value))
            }
            return Err(format!("{} Invalid assignment target {}.", self.previous()?.loc(), expr))
        }

        Ok(expr)
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
                self.consume(TokenKind::RightParen, format!("{} Expected ')' after expression, but found {:?}!", self.peek().loc(), self.peek().kind()))?;
                Ok(Expr::new_grouping(expr))
            }
            TokenKind::Var => {
                Ok(Expr::Var { ident: self.advance().clone() })
            }
            TokenKind::Identifier(_i) => {
                Ok(Expr::Var { ident: self.advance().clone() })
            }
            _ => {
                if matches!(self.peek().kind(), TokenKind::Eof) {
                    return Err(format!("{} Expected expression but found nothing (EOF).", self.peek().loc()));
                }

                match self.previous() {
                    Ok(p) => Err(format!("{} Expected Expression after {:?}, but found {:?}!", 
                        self.peek().loc(), p.kind(), self.peek().kind())),
                    Err(m) => Err(format!("{} Unexpected usage of {:?}, [dbg-msg] {}", self.peek().loc(), self.peek().kind(), m))
                }
            }
        }
    }
}
