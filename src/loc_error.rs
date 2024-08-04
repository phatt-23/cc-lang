use crate::location::Location;

#[derive(Debug, Clone)]
pub struct LocErr {
    pub loc: Location,
    pub msg: String,
}

impl LocErr {
    pub fn new(loc: &Location, msg: String) -> Self {
        Self { loc: loc.clone(), msg }
    }
}
