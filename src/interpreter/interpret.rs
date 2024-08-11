use std::{cell::RefCell, rc::Rc};

use crate::{class_entity::Class, enviroment::Environment, expression::Expr, function_entity::Function, literal_value::LitVal, loc_error::LocErr, runtime_entity::RuntimeEntity, statement::Stmt, token::{Token, TokenKind}};

use super::Interpreter;

impl Interpreter {

    pub fn interpret_class_stmt(&mut self, ident: Token, _methods: Vec<Stmt>) -> Result<Option<RuntimeEntity>, LocErr> {
        let name = match ident.kind() {
            TokenKind::Identifier(name) => name.clone(),
            _ => unreachable!("Class name must be an identifier.")  
        };
        
        let class = Class::new(name.clone());
        self.enviroment.borrow_mut().define(name, RuntimeEntity::new_from(class));
        Ok(None)
    }

    pub fn interpret_return_stmt(&mut self, keyword: Token, value: Option<Expr>) -> Result<Option<RuntimeEntity>, LocErr> {
        let evald_value = match value {
            Some(val) => match self.evaluate(val) {
                Ok(v) => v,
                Err(e) => return Err(LocErr::new(keyword.loc(), format!("Return value failed to evaluate.\n\t    [ERROR-from-return-value][{}] {}", e.loc, e.msg)))
            }
            None => RuntimeEntity::new_from(LitVal::Nil),
        };
        // This need to be cathed by the function it was called from
        return Ok(Some(evald_value))
    }

    pub fn interpret_function_stmt(&mut self, ident: Token, params: Vec<Token>, body: Vec<Stmt>) -> Result<Option<RuntimeEntity>, LocErr> {
        let function_identifer = match ident.kind() {
            TokenKind::Identifier(value) => value,
            _ => unreachable!("{} Can't use {:?} as a function identifier.", ident.loc(), ident.kind())
        };
        // get the length beforehand, because the params are moved to the closure
        // clone this enviroment, for the closure to capture
        let params_len = params.len();
        let global_env = self.globals.clone();
        let parent_env = self.enviroment.clone();
        
        let func_impl = move |args: Vec<RuntimeEntity>| -> Result<Option<RuntimeEntity>, LocErr> {
            let mut function_interp = Interpreter::for_closure(global_env.clone(), parent_env.clone());

            for (index, arg) in args.iter().enumerate() {
                let param_ident = match params[index].kind() {
                    TokenKind::Identifier(ident) => ident.clone(),
                    _ => unreachable!("Parameter in the `params` vector should be Identifier.")
                };
                function_interp.enviroment.borrow_mut()
                    .define(param_ident, arg.clone());
            }
            
            for i in 0..body.len() {
                check_return!(function_interp.interpret_stmts(vec![body[i].clone()]));
            }

            return Ok(Some(RuntimeEntity::new_from(LitVal::Nil)));
        };

        let function_declaration = Function::new(
            function_identifer.clone(), 
            params_len, 
            Rc::new(func_impl),
        );

        self.enviroment.borrow_mut()
            .define(function_identifer.clone(), RuntimeEntity::new_from(function_declaration));

        Ok(None)
    }

    pub fn interpret_var_stmt(&mut self, ident: Token, expression: Expr) -> Result<Option<RuntimeEntity>, LocErr> {
        let ident = match ident.kind() {
            TokenKind::Identifier(value) => value,
            _ => unreachable!("{} Can't use {:?} as a variable identifier.", ident.loc(), ident.kind())
        };

        let value = match self.evaluate(expression) {
            Ok(val) => val,
            Err(err) => return Err(err)
        };
        
        self.enviroment.borrow_mut().define(ident.clone(), value);

        Ok(None)
    }

    pub fn interpret_while_stmt(&mut self, condition: Expr, body: Stmt) -> Result<Option<RuntimeEntity>, LocErr> {
        let body = match body {
            Stmt::Block { statements } => statements,
            any => vec![any]
        };

        let mut cond = match self.evaluate(condition.clone()) {
            Ok(res) => match res {
                RuntimeEntity::Literal(val) => val,
                RuntimeEntity::ClassInstance(inst) => todo!("Condition must evaluate to a literal value. Instead got {}.", inst),
                RuntimeEntity::Callable(f) => todo!("Condition must evaluate to a literal value. Insted got {}.", f)
            },
            Err(err) => return Err(err)
        };

        while cond.is_truthy() == LitVal::Bool(true) {
            let a = self.interpret_stmts(body.clone());
            check_return!(a);
            
            cond = match self.evaluate(condition.clone()) {
                Ok(res) => match res {
                    RuntimeEntity::Literal(val) => val,
                    RuntimeEntity::ClassInstance(inst) => todo!("Condition must evaluate to a literal value. Instead got {}.", inst),
                    RuntimeEntity::Callable(f) => todo!("Condition must evaluate to a literal value. Insted got {}.", f)
                },
                Err(err) => return Err(err)
            }
        }

        Ok(None)
    }

    pub fn interpret_if_stmt(&mut self, condition: Expr, then_stmt: Stmt, else_stmt: Option<Box<Stmt>>) -> Result<Option<RuntimeEntity>, LocErr> {
        let cond_expr = self.evaluate(condition);

        match cond_expr {
            Ok(res) => match res {
                RuntimeEntity::Literal(val) => {
                    if matches!(val.is_truthy(), LitVal::Bool(true)) {
                        check_return!(self.interpret_stmts(vec![then_stmt]));
                    } else if let Some(else_stmt) = else_stmt {
                        check_return!(self.interpret_stmts(vec![*else_stmt]));
                    }
                }
                RuntimeEntity::ClassInstance(inst) => todo!("Condition must evaluate to a literal value. Instead got {}.", inst),
                RuntimeEntity::Callable(f) => todo!("Condition must evaluate to a literal value. Insted got {}.", f)
            }
            Err(err) => return Err(err)
        }

        Ok(None)
    }

    pub fn interpret_block_stmt(&mut self, statements: Vec<Stmt>) -> Result<Option<RuntimeEntity>, LocErr> {
        let enclosing_env = self.enviroment.clone();
        let block_env = Environment::new_local(self.enviroment.clone());                    
        
        self.enviroment = Rc::new(RefCell::new(block_env));

        check_return!(self.interpret_stmts(statements));
        
        self.enviroment = enclosing_env;

        Ok(None)
    }

    pub fn interpret_expr_stmt(&mut self, expression: Expr) -> Result<Option<RuntimeEntity>, LocErr> {
        let expr = self.evaluate(expression);
                    
        match expr {
            Ok(_) => {}
            Err(err) => return Err(err)
        }

        Ok(None)    
    }

    pub fn interpret_print_stmt(&mut self, expression: Expr) -> Result<Option<RuntimeEntity>, LocErr> {
        let expr = self.evaluate(expression);
                    
        match expr {
            Ok(val) => println!("{}", val),
            Err(err) => return Err(err)
        }

        Ok(None)
    }

}
