use crate::{enviroment::Environment, expression::Expr, function_entity::Function, literal_value::LitVal, loc_error::LocErr, runtime_entity::RuntimeEntity, statement::Stmt};

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub struct Interpreter {
    pub globals:    Rc<RefCell<Environment>>,
    pub enviroment: Rc<RefCell<Environment>>,
    pub locals:     HashMap<Expr, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut global_env = Environment::new();
        
        let f = Function::new(String::from("clock"), 0, 
            Rc::new(|_| {
                let time = std::time::SystemTime::now().duration_since(std::time::SystemTime::UNIX_EPOCH).unwrap().as_secs();
                Ok(Some(RuntimeEntity::Literal(LitVal::Int(time as i32))))
            })
        );

        global_env.define(String::from("clock"), RuntimeEntity::new_from(f));
        let global_env = Rc::new(RefCell::new(global_env));

        Self {
            globals:    global_env.clone(),
            enviroment: global_env.clone(),
            locals:     HashMap::new(),
        }
    }

    pub fn for_closure(global_env: Rc<RefCell<Environment>>, enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            globals:    global_env,
            enviroment: Rc::new(RefCell::new(Environment::new_local(enclosing))),
            locals:     HashMap::new(),
        }
    }

    fn look_up_variable(&mut self, name: &String, expr: &Expr) -> Option<RuntimeEntity> {
        let distance = self.locals.get(expr);
        match distance {
            Some(dist) => self.enviroment.as_ref().borrow().get_at(dist, name),
            None       => self.globals.as_ref().borrow().get(name)
        }
    }
}

#[macro_export]
macro_rules! check_return {
    ( $ret:expr ) => {
        match $ret {
            Ok(Some(val)) => return Ok(Some(val)),
            Ok(None)      => {},
            Err(err)      => return Err(err),
        }
    };
}

mod evaluator;
mod interpret;

impl Interpreter {
    pub fn interpret_with_resolved(&mut self, resolved_locals: HashMap<Expr, usize>, stmts: Vec<Stmt>) -> Result<Option<RuntimeEntity>, LocErr> {
        self.locals = resolved_locals;
        self.interpret_stmts(stmts)
    }

    pub fn interpret_stmts(&mut self, stmts: Vec<Stmt>) -> Result<Option<RuntimeEntity>, LocErr> {
        for stmt in stmts {
            let opt_rt_entity = match stmt {
                Stmt::Class         { ident, methods }                  => self.interpret_class_stmt(ident, methods)?,
                Stmt::Return        { keyword, value }                  => self.interpret_return_stmt(keyword, value)?,
                Stmt::Function      { ident, params, body }             => self.interpret_function_stmt(ident, params, body)?,
                Stmt::Var           { ident, expression }               => self.interpret_var_stmt(ident, expression)?,
                Stmt::While         { condition, body }                 => self.interpret_while_stmt(condition, *body)?,
                Stmt::If            { condition, then_stmt, else_stmt } => self.interpret_if_stmt(condition, *then_stmt, else_stmt)?,
                Stmt::Block         { statements }                      => self.interpret_block_stmt(statements)?,
                Stmt::Expression    { expression }                      => self.interpret_expr_stmt(expression)?,
                Stmt::Print         { expression }                      => self.interpret_print_stmt(expression)?,
            };

            if let Some(rt_entity) = opt_rt_entity {
                return Ok(Some(rt_entity))
            }

        }
        Ok(None)
    }

    pub fn evaluate(&mut self, expr: Expr) -> Result<RuntimeEntity, LocErr> {
    	match expr {
    	    Expr::Literal   { value }                           => Ok(RuntimeEntity::new_from(value)),
    	    Expr::Grouping  { expr }                            => self.evaluate(*expr),
            Expr::Set       { object, ident, value }            => self.evaluate_set_expr(*object, ident, *value),
            Expr::Get       { object, ident }                   => self.evaluate_get_expr(*object, ident),
			Expr::Call      { callee, arguments, right_paren }  => self.evaluate_call_expr(*callee, arguments, right_paren),
			Expr::Lambda    { params, body, right_paren: _ }    => self.evaluate_lambda_expr(params, body),
    	    Expr::Unary     { operator, expr }                  => self.evaluate_unary_expr(operator, *expr),
            Expr::Logical   { operator, left, right }           => self.evaluate_logical_expr(operator, *left, *right),
    	    Expr::Binary    { operator, left, right }           => self.evaluate_binary_expr(operator, *left, *right), 
            Expr::Var       { ident }                           => self.evaluate_var_expr(ident),
            Expr::Assign    { target, value }                   => self.evaluate_assign_expr(target, *value),
    	}
    }

}

