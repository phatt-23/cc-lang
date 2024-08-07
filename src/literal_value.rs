use std::rc::Rc;

use crate::loc_error::LocErr;

#[derive(Clone)]
pub enum LitVal { 
	Int(i32), 
	Double(f64), 
	String(String), 
	Bool(bool), 
	Nil,
	Callable { 
		ident: String, 
		arity: usize,
		func: Rc<dyn Fn(Vec<LitVal>) -> Result<LitVal, LocErr>>
	},
}

impl std::fmt::Debug for LitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(v) => write!(f, "Int({})", v),
            Self::Double(v) => write!(f, "Double({})", v),
            Self::String(v) => write!(f, "String({})", v),
            Self::Bool(v) => write!(f, "Bool({})", v),
            Self::Nil => write!(f, "Nil"),
            Self::Callable { ident, arity, func: _ } => write!(f, "Callable({}, {})", ident, arity),
        }
    }
}

impl std::fmt::Display for LitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LitVal::Int(v)    => write!(f, "{}", v),
			LitVal::Double(v) => write!(f, "{}", v),
			LitVal::String(v) => write!(f, "\"{}\"", v),
			LitVal::Bool(v)   => write!(f, "{}", v),
			LitVal::Nil       => write!(f, "nil"),
			LitVal::Callable { ident, arity, func: _ } => write!(f, "{}/{}", ident, arity),
        }
    }
}

impl PartialEq for LitVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(v0), Self::Int(v1)) => v0 == v1,
            (Self::Double(v0), Self::Double(v1)) => v0 == v1,
            (Self::String(v0), Self::String(v1)) => v0 == v1,
            (Self::Bool(v0), Self::Bool(v1)) => v0 == v1,
            (Self::Nil, Self::Nil) => true,
            (Self::Callable { ident: i0, arity: a0, func: _ }, Self::Callable { ident: i1, arity: a1, func: _ }) => i0 == i1 && a0 == a1,
            (any0, any1) => panic!("Can't compare values of different types. Tried to compare {} with {}", any0, any1)
        }
    }
}
