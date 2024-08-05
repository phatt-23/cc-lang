#[derive(Clone, PartialEq)]
pub struct Location {
    file: String,
    line: usize,
    col: usize,
}

impl Location {
    pub fn create(file: String, line: usize, col: usize) -> Self {
        Location { file, line: line+ 1, col: col + 1 }
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}

impl std::fmt::Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Location: {}:{}:{}", self.file, self.line, self.col)
    }
}

