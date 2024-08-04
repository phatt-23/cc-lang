use crate::token::{Token, TokenKind};
use crate::statement::Stmt;
use crate::expression::{Expr, LitVal};
use crate::loc_error::LocErr;

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
    
    // helpers --------------------------------------
    fn is_at_end(&self) -> bool {
        self.current as usize >= self.tokens.len() || matches!(self.peek().kind(), TokenKind::Eof) 
    }
    
    fn peek(&self) -> &Token {
        self.tokens.get(self.current as usize).unwrap()
    }

    fn previous(&self) -> Result<&Token, LocErr> {
        if self.is_at_end() || (self.current - 1) as usize >= self.tokens.len() {
            Err(LocErr {loc: self.peek().loc().clone(), msg: format!("Couldn't read previous token of {:?}!", self.peek().kind())})
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

    fn consume(&mut self, tok_kind: TokenKind, msg: String) -> Result<&Token, LocErr> { 
        if *self.peek().kind() == tok_kind {
            Ok(self.advance())
        } else {
            Err(LocErr {loc: self.peek().loc().clone(), msg})
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
}

impl Parser {
    pub fn parse(&mut self, tokens: Vec<Token>) -> Vec<Stmt> {
	    self.tokens = tokens;

	    let mut stmts: Vec<Stmt> = Vec::new();
	    let mut errs: Vec<LocErr> = Vec::new();

	    while !self.is_at_end() {
	        let stmt = self.declaration();
	        match stmt {
    	    	Ok(s) => stmts.push(s),
    	    	Err(err) => {
    	    	    errs.push(err);
    	    	    self.synchronize();
	            }
	        }
	    }

        for e in errs {
            println!("[ERROR][parser] {} {}", e.loc, e.msg);
        }

	    stmts
    }

    //AST Syntax Stmt -----------------------------------
    fn declaration(&mut self) -> Result<Stmt, LocErr> {
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
    
    fn var_declaration(&mut self) -> Result<Stmt, LocErr> {
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
                    format!("Expected semicolon ';' denoting end of variable declaration, instead found {:?}.", self.peek().kind())
                )?;
                Ok(Stmt::Var { ident, expression })
            }
            any => Err(LocErr {loc: self.peek().loc().clone(), msg: format!("Expected Identifier after `var` keyword, but found {:?}.", any)})
        }
    }

    fn statement(&mut self) -> Result<Stmt, LocErr> {
        // TODO: Change this to advance()
        let tok = self.peek();
	    match tok.kind() {
            TokenKind::LeftBrace => self.block_statement(),
	        TokenKind::Print => self.print_statement(),
            TokenKind::If => self.if_statement(),
            TokenKind::While => self.while_statement(),
	        _ => self.expression_statement(),
	    }
    }

    fn while_statement(&mut self) -> Result<Stmt, LocErr> {
        self.advance();

        self.consume(TokenKind::LeftParen, "Expected left parenthesis '(' after `while` keyword.".to_string())?;
        let condition = self.expression()?;
        self.consume(TokenKind::RightParen, "Expected right parenthesis ')' to close the condition expression of `while` keyword.".to_string())?;
        // TODO: Catch this error and say that while requires a body
        let body = Box::from(self.statement()?);

        Ok(Stmt::While {
            condition, body
        })
    }
    
    fn if_statement(&mut self) -> Result<Stmt, LocErr> {
        self.advance();
        self.consume(TokenKind::LeftParen, "Expected left parenthesis '(' after `if` keyword.".to_string())?;

        let condition = self.expression()?;
        
        self.consume(TokenKind::RightParen, "Expected right parenthesis ')' to close the condition expression of `if` keyword.".to_string())?;
        // TODO: Catch this error and report that if needs a then body
        let then_b = Box::from(self.statement()?);
        let else_b = if matches!(self.peek().kind(), TokenKind::Else) {
            self.advance();
            Some(Box::from(self.statement()?))
        } else {None};
        
        Ok(Stmt::If {
            condition,
            then_stmt: then_b,
            else_stmt: else_b,
        })
    }
    
    fn block_statement(&mut self) -> Result<Stmt, LocErr> {
        self.advance();
        let mut stmts: Vec<Stmt> = Vec::new();

        while !matches!(self.peek().kind(), TokenKind::RightBrace) && !self.is_at_end() {
            let stmt = self.declaration()?;
            stmts.push(stmt);
        }

        self.consume(TokenKind::RightBrace, format!("{} Expected a right brace '}}' denoting end of a block statement.", self.peek().loc()))?;
        Ok(Stmt::Block {
            statements: stmts
        })
    }

    fn print_statement(&mut self) -> Result<Stmt, LocErr> {
        self.advance();
        let expr = self.expression()?;
        self.consume(TokenKind::Semicolon, format!("{} Expected a semicolon ';' after expression denoting end of statement.", self.peek().loc()))?;
        Ok(Stmt::Print {
            expression: expr
        })
    }

    fn expression_statement(&mut self) -> Result<Stmt, LocErr> {
        let expr = self.expression()?;
        self.consume(TokenKind::Semicolon, format!("{} Expected a semicolon ';' after expression denoting end of statement.", self.peek().loc()))?;
        Ok(Stmt::Expression {
            expression: expr
        })
    }

    //AST Syntax Expr -----------------------------------
    fn expression(&mut self) -> Result<Expr, LocErr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, LocErr> {
        let expr = self.logic_or()?;

        if matches!(self.peek().kind(), TokenKind::Equal) {
            self.advance();
            let value = self.assignment()?;
            if let Expr::Var {ident} = expr {
                return Ok(Expr::new_assign(ident, value))
            }
            let loc = self.previous()?.loc().clone();
            return Err(LocErr {loc, msg: format!("Invalid assignment target {}.", expr)})
        }

        Ok(expr)
    }

    fn logic_or(&mut self) -> Result<Expr, LocErr> {
        let mut expr = self.logic_and()?;
        while matches!(self.peek().kind(), TokenKind::Or) {
            let op = self.advance().clone();
            let right = self.logic_and()?;
            expr = Expr::new_logical(expr, op, right);
        }

        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expr, LocErr> {
        let mut expr = self.equality()?;
        while matches!(self.peek().kind(), TokenKind::And) {
            let op = self.advance().clone();
            let right = self.equality()?;
            expr = Expr::new_logical(expr, op, right);
        }

        Ok(expr)
    }
    
    fn equality(&mut self) -> Result<Expr, LocErr> {
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

    fn comparison(&mut self) -> Result<Expr, LocErr> {
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

    fn term(&mut self) -> Result<Expr, LocErr> {
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

    fn factor(&mut self) -> Result<Expr, LocErr> {
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

    fn unary(&mut self) -> Result<Expr, LocErr> {
        use TokenKind::*;
        if matches!(self.peek().kind(), Bang | Minus) {
            self.advance();
            let op = self.previous()?.clone();
            let expr = self.unary()?;
            return Ok(Expr::new_unary(op, expr))
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, LocErr> {
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
                    return Err(LocErr {loc: self.peek().loc().clone(), msg: "Expected expression but found nothing (EOF).".to_string()});
                }  

                match self.previous() {
                    Ok(p) => Err(LocErr {loc: self.peek().loc().clone(), msg: format!("Expected Expression after {:?}, but found {:?}!", p.kind(), self.peek().kind())}),
                    Err(m) => Err(LocErr {loc: self.peek().loc().clone(), msg: format!("Unexpected usage of {:?}, -- {} {}", self.peek().kind(), m.loc, m.msg)})
                }
            }
        }
    }

}
