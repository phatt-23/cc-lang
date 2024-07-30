pub struct CommandLineArgs {
    pub source_files: Vec<String>
}

impl CommandLineArgs {
    pub fn new() -> Self {
        CommandLineArgs {
            source_files: vec![],
        }
    }
}

