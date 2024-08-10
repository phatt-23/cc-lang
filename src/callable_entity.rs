use crate::class_entity::Class;
use crate::function_entity::Function;
use crate::loc_error::LocErr;
use crate::runtime_entity::{RuntimeEntity, IntoRuntimeEntity};


// Every callable entity must implement the Callable trait
pub trait Callable: std::fmt::Debug + std::fmt::Display {
    fn call(&self, args: Vec<RuntimeEntity>) -> Result<Option<RuntimeEntity>, LocErr>;
    fn arity(&self) -> usize;
}

#[derive(Debug, Clone)]
pub enum CallableEntity {
    Function(Function),
    Class(Class),
}

impl IntoRuntimeEntity for CallableEntity {
    fn into_runtime_entity(self) -> RuntimeEntity {
        RuntimeEntity::Callable(self)
    }
}

impl std::fmt::Display for CallableEntity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(func) => write!(f, "{}", func),
            Self::Class(cl) => write!(f, "{}", cl),
        }
    }
}

impl Callable for CallableEntity {
    fn arity(&self) -> usize {
        match self {
            Self::Class(c) => c.arity(),
            Self::Function(f) => f.arity(),
        }
    }

    fn call(&self, args: Vec<RuntimeEntity>) -> Result<Option<RuntimeEntity>, LocErr> {
        match self {
            Self::Class(c) => c.call(args),
            Self::Function(f) => f.call(args),
        }
    }
}




