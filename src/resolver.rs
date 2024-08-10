use std::collections::HashMap;

use crate::{expression::Expr, loc_error::LocErr, statement::Stmt, token::{Token, TokenKind}};

type ResolvedLocals = HashMap<Expr, usize>;

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    locals: ResolvedLocals,
    current_function: FunctionType,
}

#[derive(PartialEq, Copy, Clone)]
enum FunctionType {
    Function, 
    None,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            locals: HashMap::new(),
            current_function: FunctionType::None,
        }
    }

    pub fn resolve(&mut self, statements: &Vec<Stmt>) -> Result<ResolvedLocals, String> {
        let mut errors: Vec<LocErr> = Vec::new();

        for stmt in statements {
            match self.resolve_stmt(stmt) {
                Ok(_) => {},
                Err(err) => errors.push(err)
            }
        }
        
        if !errors.is_empty() {
            for e in errors {
                println!("[ERROR][Resolver][{}] {}", e.loc, e.msg);
            }
            return Err(String::from("[ERROR][Resolver][***] Error occured while resolving (static analysis)."))
        }

        Ok(self.locals.clone())
    }

    fn resolve_stmts(&mut self, statements: &Vec<Stmt>) -> Result<(), LocErr> {
        for stmt in statements {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, statement: &Stmt) -> Result<(), LocErr> {
        match statement {
            Stmt::Class { ident, methods }                  => self.resolve_class_stmt(ident, methods)?,
            Stmt::Block { statements }                      => self.resolve_block_stmt(statements)?,
            Stmt::Expression { expression }                 => self.resolve_expr(expression)?,
            Stmt::Function { ident, params, body }          => self.resolve_function_stmt(ident, params, body)?,
            Stmt::If { condition, then_stmt, else_stmt }    => self.resolve_if_stmt(condition, then_stmt, else_stmt)?,
            Stmt::Print { expression }                      => self.resolve_print_stmt(expression)?,
            Stmt::Return { keyword, value }                 => self.resolve_return_stmt(keyword, value)?,
            Stmt::Var { ident, expression }                 => self.resolve_var_stmt(ident, expression)?,
            Stmt::While { condition, body }                 => self.resolve_while_stmt(condition, body)?,
        }
        Ok(())
    }

    fn resolve_class_stmt(&mut self, ident: &Token, methods: &Vec<Stmt>) -> Result<(), LocErr> {
        self.declare(ident)?;
        self.define(ident)?;

        Ok(())
    }

    fn resolve_while_stmt(&mut self, condition: &Expr, body: &Stmt) -> Result<(), LocErr> {
        self.resolve_expr(condition)?;
        self.resolve_stmt(body)
    }
        
    fn resolve_return_stmt(&mut self, keyword: &Token, value: &Option<Expr>) -> Result<(), LocErr> {
        if self.current_function == FunctionType::None {
            return Err(LocErr::new(keyword.loc(), "Can't return from top-level scope. Found return not in function.".to_string()))
        }

        match value {
            Some(val) => self.resolve_expr(val),
            None => Ok(())
        }
    }

    fn resolve_print_stmt(&mut self, expression: &Expr) -> Result<(), LocErr> {
        self.resolve_expr(expression)
    }

    fn resolve_if_stmt(&mut self, condition: &Expr, then_stmt: &Stmt, else_stmt: &Option<Box<Stmt>>) -> Result<(), LocErr> {
        self.resolve_expr(condition)?;
        self.resolve_stmt(then_stmt)?;
        match else_stmt {
            Some(else_b) => self.resolve_stmt(else_b)?,
            None => {}
        }
        Ok(())
    }

    fn resolve_function_stmt(&mut self, ident: &Token, params: &Vec<Token>, body: &Vec<Stmt>) -> Result<(), LocErr> {
        self.declare(ident)?;
        self.define(ident)?;

        self.begin_scope();
        let enclosing_func_type = self.current_function;
        self.current_function = FunctionType::Function;
        
        for param in params {
            self.declare(param)?;
            self.define(param)?;
        }
        self.resolve_block_stmt(body)?;

        self.end_scope();
        self.current_function = enclosing_func_type;

        Ok(())
    }

    fn resolve_block_stmt(&mut self, statements: &Vec<Stmt>) -> Result<(), LocErr> {
        self.begin_scope();
        self.resolve_stmts(statements)?;
        self.end_scope();
        Ok(())
    }

    fn begin_scope(&mut self) {
        let new_scope: HashMap<String, bool> = HashMap::new();
        self.scopes.push(new_scope);
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_var_stmt(&mut self, ident: &Token, expression: &Expr) -> Result<(), LocErr> {
        self.declare(ident)?;
        self.resolve_expr(expression)?;
        self.define(ident)?;
        Ok(())
    }

    fn resolve_expr(&mut self, expression: &Expr) -> Result<(), LocErr> {
        match expression {
            Expr::Set { object, ident: _, value } => {
                self.resolve_expr(object)?;
                self.resolve_expr(value)?;
                Ok(())
            }
            Expr::Get { object, ident: _ } => {
                self.resolve_expr(object)?;
                Ok(())
            }
            Expr::Assign { target, value } => {
                let name = match target.kind() {
                    TokenKind::Identifier(name) => name,
                    _ => unreachable!("Must be identifier.")  
                };
                    
                self.resolve_expr(&**value)?;
                self.resolve_local(value, name)?;

                Ok(())
            },
            Expr::Binary { operator: _, left, right } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            },
            Expr::Call { callee, arguments, right_paren: _ } => {
                self.resolve_expr(callee)?;
                for arg in arguments {
                    self.resolve_expr(arg)?
                }
                Ok(())
            },
            Expr::Grouping { expr } => self.resolve_expr(expr),
            Expr::Lambda { params, body, right_paren: _ } => {
                self.begin_scope();
                for param in params {
                    self.declare(&param)?;
                    self.define(&param)?;
                }
                self.resolve_block_stmt(body)?;
                self.end_scope();
                Ok(())
            },
            Expr::Literal { value: _ } => Ok(()),
            Expr::Logical { operator: _, left, right } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            },
            Expr::Unary { operator: _, expr } => self.resolve_expr(expr),
            Expr::Var { ident } => {
                let name = match ident.kind() {
                TokenKind::Identifier(name) => name,
                _ => unreachable!("Must be identifier.")  
                };

                if !self.scopes.is_empty() && self.scopes.last_mut().unwrap().get(name).is_some_and(|x| *x == false) {
                    return Err( LocErr::new(&ident.loc(), String::from("Can't read local variable in its own initializer.")) )
                }
                
                self.resolve_local(expression, name)?;

                Ok(())
            },
        }
    }

    fn resolve_local(&mut self, expression: &Expr, ident: &String) -> Result<(), LocErr> {
        for index in self.scopes.len() - 1..=0 {
            if self.scopes.get(index).unwrap().contains_key(ident) {
                self.locals.insert(expression.clone(), self.scopes.len() - 1 - index);
                return Ok(())
            }
        }
        Ok(())
    }

    fn declare(&mut self, ident: &Token) -> Result<(), LocErr> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let name = match ident.kind() {
            TokenKind::Identifier(name) => name.clone(),
            _ => unreachable!("Should be identifier")
        };

        if self.scopes.last_mut().unwrap().contains_key(&name) {
            return Err(LocErr::new(ident.loc(), "There already exists a variable with this name within the scope.".to_string()))
        }

        self.scopes.last_mut().unwrap().insert(name, false);
        Ok(())
    }

    fn define(&mut self, ident: &Token) -> Result<(), LocErr> {
        if self.scopes.is_empty() {
            return Ok(())
        }

        let name = match ident.kind() {
            TokenKind::Identifier(name) => name.clone(),
            _ => unreachable!("Should be an identifier.")
        };

        self.scopes.last_mut().unwrap().insert(name, true);
        Ok(())
    }

}
