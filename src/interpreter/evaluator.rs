use std::rc::Rc;

use crate::{callable_entity::{Callable, CallableEntity}, expression::Expr, function_entity::Function, literal_value::LitVal, loc_error::LocErr, runtime_entity::RuntimeEntity, statement::Stmt, token::{Token, TokenKind}};

use super::Interpreter;

impl Interpreter {

    pub fn evaluate_set_expr(&mut self, object: Expr, ident: Token, value: Expr) -> Result<RuntimeEntity, LocErr> {
        // dbg!(&object, &ident, &value);

        let obj_name = match object {
            Expr::Var { ref ident } => {
                match ident.kind() {
                    TokenKind::Identifier(val) => val,
                    _ => todo!()
                }
            }
            _ => todo!()
        };

        //println!("-------------------------------");
        //This creates a new class instance not our class instance
        // let mut obj = match self.evaluate(object)? {
        //     RuntimeEntity::Literal(lit)        => todo!("Expected the object to evaluate to a class instance, insted got {}.", lit),
        //     RuntimeEntity::Callable(cal)       => todo!("Expected the object to evaluate to a class instance, insted got {}.", cal),
        //     RuntimeEntity::ClassInstance(inst) => inst,
        // };
        // dbg!(&obj);

        let name = match ident.kind() {
            TokenKind::Identifier(n) => n,
            _ => unreachable!("Should be an identifier.")
        };

        

        let evald_val = self.evaluate(value)?;
        let evald_val_c = evald_val.clone();
        let mut obj = match self.enviroment.borrow_mut().remove(obj_name).unwrap() {
            RuntimeEntity::ClassInstance(inst) => inst,
            _ => todo!()
        };

        obj.set(name.clone(), evald_val);
        self.enviroment.borrow_mut().define(obj_name.clone(), RuntimeEntity::new_from(obj));

        Ok(evald_val_c)
    }

    pub fn evaluate_get_expr(&mut self, object: Expr, ident: Token) -> Result<RuntimeEntity, LocErr> {
        // dbg!(&object, &ident);
        // println!("-------------------------------");
                
        let fmtd_object = format!("{}", object);

        let mut obj = match self.evaluate(object)? {
            RuntimeEntity::Literal(lit) => todo!("Expected the object to evaluate to a class instance, insted got {}.", lit),
            RuntimeEntity::Callable(cal) => todo!("Expected the object to evaluate to a class instance, insted got {}.", cal),
            RuntimeEntity::ClassInstance(inst) => inst,
        };

        let name = match ident.kind() {
            TokenKind::Identifier(n) => n,
            _ => unreachable!("Should be an identifier.")
        };

        let property = match obj.get(name) {
            Ok(prop) => prop,
            Err(msg) => return Err(LocErr::new(ident.loc(), format!("Failed execution of `get` from {}.\n    [ERROR_from_get_property] {}", fmtd_object, msg)))
        };
        
        Ok(property)
    }

    pub fn evaluate_lambda_expr(&mut self, params: Vec<Token>, body: Vec<Stmt>) -> Result<RuntimeEntity, LocErr> {
        let lambda_indentifier = format!("Lambda_{:x}", rand::random::<usize>());
        let lambda_arity = params.len();
                
        // clone them to this scope for the closure to capture
        let params = params.clone();
        let body = body.clone();
        let global_env = self.globals.clone();
        let env = self.enviroment.clone();
                
        // define the lambda
        let lambda_impl = move |args: Vec<RuntimeEntity>| -> Result<Option<RuntimeEntity>, LocErr> {
            let mut lambda_interp = Interpreter::for_closure(global_env.clone(), env.clone());
            
            for (index, arg) in args.iter().enumerate() {
                let param_ident = match params[index].kind() {
                    TokenKind::Identifier(ident) => ident.clone(),
                    _ => unreachable!("Parameter in the `params` vector should be Identifier.")
                };
                lambda_interp.enviroment.borrow_mut()
                .define(param_ident, arg.clone());
            }
            
            for i in 0..body.len() {
                check_return!(lambda_interp.interpret_stmts(vec![body[i].clone()]));
            }
            return Ok(Some(RuntimeEntity::new_from(LitVal::Nil)))
        };

        Ok(RuntimeEntity::Callable(
            CallableEntity::Function(
                Function::new(lambda_indentifier, lambda_arity, Rc::new(lambda_impl)))))
    }

    pub fn evaluate_call_expr(&mut self, callee: Expr, arguments: Vec<Expr>, right_paren: Token) -> Result<RuntimeEntity, LocErr> {
        // if the call fails, pass these to an error message
        let fmtd_args: Vec<_> = arguments.iter().map(|arg| format!("{}", arg)).collect();
        let fmtd_args = fmtd_args.join(", ");
        let callable = match self.evaluate(callee)? {
            RuntimeEntity::Callable(cl) => cl,
            RuntimeEntity::Literal(lit) => todo!("Can't call an object that is doesn't implement Callable trait {}.", lit),
            RuntimeEntity::ClassInstance(inst) => todo!("Can't call an object that is doesn't implement Callable trait {}.", inst),
        };

        let arguments_count = arguments.len();
        let mut evaled_args = vec![];
        for arg in arguments {
            let evaled_arg = self.evaluate(arg)?;
            evaled_args.push(evaled_arg);
        }  

        match &callable {
            CallableEntity::Function(fun) => {
                if fun.arity as usize != arguments_count {
                    return Err(LocErr::new(right_paren.loc(), format!("Callable `{}` expects {} arguments, but received {}. Arity check failed.", &fun.ident, &fun.arity, arguments_count)))
                }
                let call_result = (fun.func)(evaled_args);
                match call_result {
                    Ok(ret) => Ok(ret.expect("Every callable should at least return Nil. Every function returns nil implicitly.")),
                    Err(err) => {
                        Err(LocErr::new(
                            right_paren.loc(), 
                            format!("Failed execution of callable `{}` with ({}) as passed arguments.\n  [ERROR-from-callable][{}] {}", &fun.ident, fmtd_args, err.loc, err.msg)
                        ))
                    }
                }
            }
            CallableEntity::Class(cl) => {
                if cl.arity() as usize != arguments_count {
                    return Err(LocErr::new(
                        right_paren.loc(), 
                        format!("Callable `{}` expects {} arguments, but received {}. Arity check failed.", &cl.name, &cl.arity(), arguments_count)
                    ))
                }
                let call_result = cl.call(evaled_args);
                match call_result {
                    Ok(ret) => Ok(ret.expect("Every callable should at least return Nil. Every function returns nil implicitly.")),
                    Err(err) => {
                        Err(LocErr::new(
                            right_paren.loc(), 
                            format!("Failed execution of callable `{}` with ({}) as passed arguments.\n  [ERROR-from-callable][{}] {}", &cl.name, fmtd_args, err.loc, err.msg)
                        ))
                    }
                }
            }
        }
    }

    pub fn evaluate_unary_expr(&mut self, operator: Token, expr: Expr) -> Result<RuntimeEntity, LocErr> {
        let right = match self.evaluate(expr)? {
            RuntimeEntity::Literal(lit) => lit,
            RuntimeEntity::Callable(cl) => return Err(LocErr::new(operator.loc(), format!("Can't apply unary operators to callable object {}.", cl))),
            RuntimeEntity::ClassInstance(inst) => return Err(LocErr::new(operator.loc(), format!("Can't apply unary operators to callable object {}.", inst)))
        };

        match (right, operator.kind()) {
            (LitVal::Int(x), TokenKind::Minus) => Ok(RuntimeEntity::new_from(LitVal::Int(-x))),
            (LitVal::Double(x), TokenKind::Minus) => Ok(RuntimeEntity::new_from(LitVal::Double(-x))),
            (any, TokenKind::Minus) => Err(LocErr::new(operator.loc(), format!("Minus operator not implemented for {:?}", any))),
            (any, TokenKind::Bang) => Ok(RuntimeEntity::new_from(any.is_falsy())),
            _ => todo!()
        }
    }

    pub fn evaluate_assign_expr(&mut self, target: Token, value: Expr) -> Result<RuntimeEntity, LocErr> {
        if let TokenKind::Identifier(i) = target.kind() {
            let v = self.evaluate(value)?;
            return match self.enviroment.as_ref().borrow_mut().assign(i, v.clone()) {
                Some(_) => Ok(v),
                None => Err(LocErr::new(target.loc(), format!("Can't assign {} to an undeclared target `{}`.", v, i)))
            }
        }
        unreachable!("{} Target must be an identifier", target.loc());
    }

    pub fn evaluate_var_expr(&mut self, ident: Token) -> Result<RuntimeEntity, LocErr>  {
        if let TokenKind::Identifier(name) = ident.kind() {
            if let Some(val) = self.look_up_variable(name, &Expr::Var { ident: ident.clone() }) {
                return Ok(val)
            }

            match self.enviroment.as_ref().borrow().get(name) {
                Some(value) => return Ok(value.clone()),
                None => return Err(LocErr::new(ident.loc(), format!("Variable with identifier `{}` is undeclared.", name)))
            }
        }
        unreachable!("{} Totally fucked up. {:?} is not an identifier.", ident.loc(), ident.kind());
    }

    pub fn evaluate_logical_expr(&mut self, operator: Token, left: Expr, right: Expr) -> Result<RuntimeEntity, LocErr> {
        let left = match self.evaluate(left)? {
            RuntimeEntity::Literal(lit) => lit,
            RuntimeEntity::Callable(func) => return Err(LocErr::new(operator.loc(), format!("Left exression must evaluate to a literal value. Instead evaluated to {}.", func))),
            RuntimeEntity::ClassInstance(inst) => return Err(LocErr::new(operator.loc(), format!("Left exression must evaluate to a literal value. Instead evaluated to {}.", inst))),
        };

        match operator.kind() {
            TokenKind::And => {
                if left.is_truthy() == LitVal::Bool(false) {
                    return Ok(RuntimeEntity::new_from(left));
                }
            }
            TokenKind::Or => {
                if left.is_truthy() == LitVal::Bool(true) {
                    return Ok(RuntimeEntity::new_from(left))
                }
            }
            _ => unreachable!("Can't perform a logical evaluation with non-logical operations. {:?}", operator)
        }

        Ok(self.evaluate(right)?)
    }

    pub fn evaluate_binary_expr(&mut self, operator: Token, left: Expr, right: Expr) -> Result<RuntimeEntity, LocErr> {
        use LitVal::*;
        let left = match self.evaluate(left)? {
            RuntimeEntity::Literal(lit) => lit,
            RuntimeEntity::Callable(func) => return Err(LocErr::new(operator.loc(), format!("Left exression must evaluate to a literal value. Instead evaluated to {}.", func))),
            RuntimeEntity::ClassInstance(isnt) => return Err(LocErr::new(operator.loc(), format!("Left exression must evaluate to a literal value. Instead evaluated to {}.", isnt))),
        };

        let right = match self.evaluate(right)? {
            RuntimeEntity::Literal(lit) => lit,
            RuntimeEntity::Callable(func) => return Err(LocErr::new(operator.loc(), format!("Right exression must evaluate to a literal value. Instead evaluated to {}.", func))),
            RuntimeEntity::ClassInstance(inst) => return Err(LocErr::new(operator.loc(), format!("Right exression must evaluate to a literal value. Instead evaluated to {}.", inst))),
        };

        match operator.kind() {
            TokenKind::Minus => {
                match (left, right) {
                    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Double(x - y))),
                    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Int(x - y))),
                    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Int(x as i32 - y))),
                    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Int(x - y as i32))),
                    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical values.", operator.kind(), l, r)))
                }
            }
            TokenKind::Plus => {
                match (left, right) {
                    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Double(x + y))),
                    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Int(x + y))),
                    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Int(x as i32 + y))),
                    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Int(x + y as i32))),
                    (String(x), String(y))  => Ok(RuntimeEntity::new_from(String(format!("{}{}", x, y)))),
                    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical or concatenable.", operator.kind(), l, r)))
                }
            }
            TokenKind::Star => {
                match (left, right) {
                    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Double(x * y))),
                    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Int(x * y))),
                    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Double(x * f64::from(y)))),
                    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Double(f64::from(x) * y))),
                    (l, r) => {
                        Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
                    }
                }
            }
            TokenKind::Slash => {
                match (left, right) {
                    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Double(x / y))),
                    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Int(x / y))),
                    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Double(x / f64::from(y)))),
                    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Double(f64::from(x) / y))),
                    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
                }
            }
            TokenKind::Greater => {
                match (left, right) {
                    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Bool(x > y))),
                    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Bool(x > y))),
                    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Bool(x > f64::from(y)))),
                    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Bool(f64::from(x) > y))),
                    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
                }
            }
            TokenKind::Less => {
                match (left, right) {
                    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Bool(x < y))),
                    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Bool(x < y))),
                    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Bool(x < f64::from(y)))),
                    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Bool(f64::from(x) < y))),
                    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
                }
            }
            TokenKind::GreaterEqual => {
                match (left, right) {
                    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Bool(x >= y))),
                    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Bool(x >= y))),
                    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Bool(x >= f64::from(y)))),
                    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Bool(f64::from(x) >= y))),
                    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
                }
            }
            TokenKind::LessEqual => {
                match (left, right) {
                    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Bool(x <= y))),
                    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Bool(x <= y))),
                    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Bool(x <= f64::from(y)))),
                    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Bool(f64::from(x) <= y))),
                    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical.", operator.kind(), l, r)))
                }
            }
            TokenKind::EqualEqual => {
                match (left, right) {
                    (Bool(x), Bool(y))      => Ok(RuntimeEntity::new_from(Bool(x == y))),
                    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Bool(x == y))),
                    (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Bool(x == y))),
                    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Bool(x == f64::from(y)))),
                    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Bool(f64::from(x) == y))),
                    (String(x), String(y))  => Ok(RuntimeEntity::new_from(Bool(x == y))),
                    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on {} and {}. Operands must be numerical or boolean", operator.kind(), l, r)))
                }
            }
            TokenKind::BangEqual => {
                match (left, right) {
                    (Bool(x), Bool(y))      => Ok(RuntimeEntity::new_from(Bool(x != y))),
                    (Double(x), Double(y))  => Ok(RuntimeEntity::new_from(Bool(x != y))),
                     (Int(x), Int(y))        => Ok(RuntimeEntity::new_from(Bool(x != y))),
                    (Double(x), Int(y))     => Ok(RuntimeEntity::new_from(Bool(x != f64::from(y)))),
                    (Int(x), Double(y))     => Ok(RuntimeEntity::new_from(Bool(f64::from(x) != y))),
                    (String(x), String(y))  => Ok(RuntimeEntity::new_from(Bool(x != y))),
                    (l, r) => Err(LocErr::new(operator.loc(), format!("Can't perform {:?} operation on non-numerical or non-boolean values {} and {}", operator.kind(), l, r)))
                }
            }
            TokenKind::And | TokenKind::Or => unreachable!("Logical operations are not no be processed by the Binary branch."),
            e => Err(LocErr::new(operator.loc(), format!("Undefined binary operator {}.", e)))
        }
    }
    
}
