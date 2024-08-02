use crate::parser::{Expr, LitVal, Parser};

pub struct Interpreter {

}

impl Interpreter {
    pub fn new() -> Self {
	Self {
	}
    }

    pub fn interpret(&mut self, expr: Expr) -> Result<LitVal, String> {
	expr.evaluate()
    }

}
