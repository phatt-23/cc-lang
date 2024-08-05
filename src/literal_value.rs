use std::rc::Rc;


#[derive(Clone)]
pub enum LitVal { 
	Int(i32), 
	Double(f64), 
	String(String), 
	Bool(bool), 
	Nil,
	Callable { 
		ident: String, 
		arity: u8,
		func: Rc<dyn Fn(Vec<LitVal>) -> LitVal>
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

impl PartialEq for LitVal {
    fn eq(&self, other: &Self) -> bool {
        // TODO: Can you compare two variants that are not of the same type? 
        // TODO: Do you panic or just return false? Maybe we can compare 
        // TODO: numbers (int, float) and bools and nils with each other.
        match self {
            Self::Int(val) => if let Self::Int(val_other) = other { val == val_other } else { panic!("Can't compare two LitVals that are not both of Int variant") }
            Self::Double(val) => if let Self::Double(val_other) = other {val == val_other} else { panic!("Can't compare two LitVals that are not both of Double variant") }
            Self::String(val) => if let Self::String(val_other) = other {val == val_other} else { panic!("Can't compare two LitVals that are not both of String variant") }
            Self::Bool(val) => if let Self::Bool(val_other) = other {val == val_other} else { panic!("Can't compare two LitVals that are not both of Bool variant") }
            Self::Nil => matches!(other, Self::Nil),
            Self::Callable { ident: i, arity: a, func: _ } => {
                if let Self::Callable { ident: i_other, arity: a_other, func: _ } = other {
                    return i == i_other && a == a_other
                } else {
                    false
                }
            }
        }
    }
}
