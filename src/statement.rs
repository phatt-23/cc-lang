use crate::expression::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Stmt {
    Expression  { expression: Expr },
    Print       { expression: Expr },
    Var         { ident: Token, expression: Expr },
    Block       { statements: Vec<Stmt> },
    If          { condition: Expr, then_stmt: Box<Stmt>, else_stmt: Option<Box<Stmt>> },
    While       { condition: Expr, body: Box<Stmt> },
    Function    { ident: Token, params: Vec<Token>, body: Vec<Stmt> },
    Return      { keyword: Token, value: Option<Expr> },
    Class       { ident: Token, methods: Vec<Stmt> } // methods: Vec<Stmt::Function>
}

impl Stmt {
    pub fn new_expr(expression: Expr) -> Self {
        Self::Expression { expression }
    }

    pub fn new_print(expression: Expr) -> Self {
        Self::Print { expression }
    }

    pub fn new_var(ident: Token, expression: Expr) -> Self {
        Self::Var { ident, expression }
    }

    pub fn new_block(statements: Vec<Stmt>) -> Self {
        Self::Block { statements }
    }

    pub fn new_if(condition: Expr, then_stmt: Box<Stmt>, else_stmt: Option<Box<Stmt>>) -> Self {
        Self::If { condition, then_stmt, else_stmt }
    }

    pub fn new_while(condition: Expr, body: Box<Stmt>) -> Self {
        Self::While { condition, body }
    }

    pub fn new_function(ident: Token, params: Vec<Token>, body: Vec<Stmt>) -> Self {
        Self::Function { ident, params, body }
    }

    pub fn new_return(keyword: Token, value: Option<Expr>) -> Self {
        Self::Return {  keyword, value }
    }

    pub fn new_class(ident: Token, methods: Vec<Stmt>) -> Self {
        Self::Class { ident, methods }
    }
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Class { ident, methods } => {
                dbg!(ident, methods);
                todo!();
            }
            Stmt::Return { keyword: _, value } => {
                if let Some(value) = value {
                    return write!(f, "(:return {})", value)
                }
                write!(f, "(:return)")
            }
            Stmt::Function { ident, params, body } => {
                let mut params_fmtd = String::new();
                let body_fmtd = format!("{}", Stmt::new_block( body.iter().map(|b| b.clone()).collect() ));

                // TODO: Reformat into more functional code
                for (index, param) in params.iter().enumerate() {
                    let param_fmtd = format!("{}", param.kind());
                    params_fmtd.push_str(param_fmtd.as_str());
                    
                    if index < params.len() - 1 {
                        params_fmtd.push(' ');
                    }
                }

                write!(f, "(:defun {} ({}) {})", ident.kind(), params_fmtd, body_fmtd)
            }
            Stmt::Expression { expression } => {
                write!(f, "{}", expression)
            }
            Stmt::Var { ident, expression } => {
                write!(f, "(:defvar {} {})", ident.kind(), expression)
            }
            Stmt::Print { expression } => {
                write!(f, "(:print {})", expression)
            }
            Stmt::Block { statements } => {
                let mut disp_out = String::new();
                
                disp_out.push_str("(:begin");
                for stmt in statements {
                    disp_out.push_str(format!(" {}", stmt).as_str());
                }
                disp_out.push_str(" :end)");

                write!(f, "{}", disp_out)
            }
            Stmt::If { condition, then_stmt, else_stmt } => {
                let mut disp_out = String::new();

                disp_out.push_str(format!("(:if {} :then {}", condition, then_stmt).as_str());
                if let Some(else_st) = else_stmt {
                    disp_out.push_str(format!(" :else {}", *else_st).as_str());
                } 
                disp_out.push_str(" :fi)");

                write!(f, "{}", disp_out)
            }
            Stmt::While { condition, body } => {
                write!(f, "(:while {} :do {} :done)", condition, body)
            }
        }
    }
}
