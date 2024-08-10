use std::hash::{Hash, Hasher};

use crate::runtime_entity::{RuntimeEntity, IntoRuntimeEntity};

#[derive(Clone)]
pub enum LitVal { 
	Int(i32), 
	Double(f64), 
	String(String), 
	Bool(bool), 
	Nil,
}

impl IntoRuntimeEntity for LitVal {
    fn into_runtime_entity(self) -> RuntimeEntity {
        RuntimeEntity::Literal(self)
    }
}

impl PartialEq for LitVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l), Self::Bool(r)) => l == r,
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
            Self::Double(f) => {
                let bits = f.to_bits();
                bits.hash(state);
            }
            Self::Int(i) => i.hash(state),
            Self::Nil => 0_u8.hash(state),
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
        }
    }
}

impl std::fmt::Display for LitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Int(v)    => write!(f, "{}", v),
			Self::Double(v) => write!(f, "{}", v),
			Self::String(v) => write!(f, "{}", v),
			Self::Bool(v)   => write!(f, "{}", v),
			Self::Nil       => write!(f, "nil"),
        }
    }
}
