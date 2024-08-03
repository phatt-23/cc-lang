use crate::expression::Expr;
use crate::token::Token;

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression { expression: Expr },
    Print { expression: Expr },
    Var { ident: Token, expression: Expr },
}
