use crate::expression::Expr;
use crate::token::Token;

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression { expression: Expr },
    Print { expression: Expr },
    Var { ident: Token, expression: Expr },
    Block { statements: Vec<Stmt> },
    If { condition: Expr, then_stmt: Box<Stmt>, else_stmt: Option<Box<Stmt>> },
    While { condition: Expr, body: Box<Stmt> },
}
