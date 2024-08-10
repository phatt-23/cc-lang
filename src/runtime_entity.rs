use crate::{callable_entity::CallableEntity, class_entity::ClassInstance, literal_value::LitVal};

pub trait IntoRuntimeEntity {
    fn into_runtime_entity(self) -> RuntimeEntity;
}

#[derive(Debug, Clone)]
pub enum RuntimeEntity {
    Literal(LitVal),
    Callable(CallableEntity),
    ClassInstance(ClassInstance),
}

impl RuntimeEntity {
    pub fn new_from<T>(entity: T) -> Self
    where T: IntoRuntimeEntity {
        entity.into_runtime_entity()
    }
}

impl std::fmt::Display for RuntimeEntity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(lit) => write!(f, "{}", lit),
            Self::Callable(func) => write!(f, "{}", func),
            Self::ClassInstance(inst) => write!(f, "{}", inst)
        }
    }
}