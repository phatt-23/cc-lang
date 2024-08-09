use std::rc::Rc;
use std::hash::{Hash, Hasher};

use crate::interpreter::InterpReturn;
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
		func: Rc<dyn Fn(Vec<LitVal>) -> Result<Option<InterpReturn>, LocErr>>
	},
}

impl PartialEq for LitVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l), Self::Bool(r)) => l == r,
            (Self::Callable { ident: l_ident, arity: l_arity, func: _ }, Self::Callable { ident: r_ident, arity: r_arity, func: _ }) => l_ident == r_ident && l_arity == r_arity,
            (Self::Double(l), Self::Double(r)) => l == r,
            (Self::Int(l), Self::Int(r)) => l == r,
            (Self::Nil, Self::Nil) => true,
            (Self::String(l), Self::String(r)) => l == r,
            _ => false
        }        
    }
}

impl Eq for LitVal {
    fn assert_receiver_is_total_eq(&self) {}
}


impl Hash for LitVal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Bool(b) => b.hash(state),
            Self::Callable { ident, arity, func: _ } => {
                ident.hash(state);
                arity.hash(state);
            }
            Self::Double(f) => {
                let bits = f.to_bits();
                bits.hash(state);
            }
            Self::Int(i) => i.hash(state),
            Self::Nil => ().hash(state),
            Self::String(s) => s.hash(state),
        }
    }
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
			LitVal::String(v) => write!(f, "{}", v),
			LitVal::Bool(v)   => write!(f, "{}", v),
			LitVal::Nil       => write!(f, "nil"),
			LitVal::Callable { ident, arity, func: _ } => write!(f, "{}/{}", ident, arity),
        }
    }
}
