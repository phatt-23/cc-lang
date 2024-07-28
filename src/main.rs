use std::fs::{self, File};
use std::io::{self, Error, BufRead, BufReader};
use std::path;

struct CommandLineArgs {
    source_files: Vec<String>
}

impl CommandLineArgs {
    pub fn new() -> Self {
        CommandLineArgs {
            source_files: vec![],
        }
    }
}

struct Location {
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

#[derive(Debug, PartialEq, PartialOrd)]
enum TokenKind {
    // Single
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    // One or two characters
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    // Literals
    Identifier, String(String), Int(i32), Double(f64),
    // Keyword
    Class, Fun, Return,
    If, Else, While, For, 
    False, True, Nil, 
    And, Or, 
    Print,
    This, Super,
    Var,

    Eof,
}

struct Token {
    kind: TokenKind,
    loc: Location,
}

impl Token {
    pub fn create(kind: TokenKind, loc: Location) -> Self {
        Token { kind, loc }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?}", self.loc, self.kind)
    }
}

struct Scanner {
    source: String,
    tokens: Vec<Token>,
    file: String,
    line: usize,
    col: usize,
    current: usize,
    err_reports: Vec<(Location, String)>,
}

impl Scanner {
    pub fn new() -> Self {
        Scanner {
            tokens: Vec::new(),
            source: String::new(),
            file: String::new(),
            line: 0,
            col: 0,
            current: 0,
            err_reports: Vec::new(),
        }
    }

    fn report_err(&mut self, loc: Location, message: String) {
        self.err_reports.push((loc, message));
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.col += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0'
        }
        self.source.chars().nth(self.current).unwrap() 
    }
    
    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0'
        }
        self.source.chars().nth(self.current + 1).unwrap() 
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected  {
            return false
        }
        self.col += 1;
        self.current += 1;
        true
    }

    fn add_token(&mut self, tok_kind: TokenKind) {
        let loc = Location::create(self.file.clone(), self.line, self.col);
        let tok = Token::create(tok_kind, loc);
        self.tokens.push(tok);
    }

    fn handle_string(&mut self) {
        let start = self.current;
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.report_err(Location::create(self.file.clone(), self.line, self.col), "Unterminated string".to_string())
        }

        let str = self.source.get(start..self.current).unwrap().to_string();
        self.advance();
        self.add_token(TokenKind::String(str));
    }

    fn handle_number(&mut self) {
        let start = self.current - 1;
        let mut is_real = false;
        while self.peek().is_ascii_digit() && !self.is_at_end() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            is_real = true;
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let loc = Location::create(self.file.clone(), self.line, self.col);
        let sub_str = self.source.get(start..self.current).unwrap().to_string();

        if is_real {
            let number = sub_str.parse::<f64>().unwrap_or_else(|_| {
                self.report_err(loc, format!("Number failed to parse ({})", sub_str));
                0.0_f64
            });
            self.add_token(TokenKind::Double(number));
        } else {
            let number = sub_str.parse::<i32>().unwrap_or_else(|_| {
                self.report_err(loc, format!("Number failed to parse ({})", sub_str));
                0_i32
            });
            self.add_token(TokenKind::Int(number));
        }
    }

    pub fn scan(&mut self, filepath: String) {
        let source: String = fs::read_to_string(&filepath).unwrap();
        self.file = filepath;
        self.source = source;
        while !self.is_at_end() {
            self.scan_token()
        }

        for t in &self.tokens {
            println!("[INFO] {}", t);
        }

        for e in &self.err_reports {
            println!("[ERROR] {} {}", e.0, e.1);
        }
    }

    fn scan_token(&mut self) {
        let loc = Location::create(self.file.clone(), self.line, self.col);
        let c: char = self.advance();
        match c {
        //Single character
            '(' => self.add_token(TokenKind::LeftParen),
            ')' => self.add_token(TokenKind::RightParen),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            ',' => self.add_token(TokenKind::Comma),
            '.' => self.add_token(TokenKind::Dot),
            ';' => self.add_token(TokenKind::Semicolon),
            '+' => self.add_token(TokenKind::Plus),
            '-' => self.add_token(TokenKind::Minus),
            '*' => self.add_token(TokenKind::Star),
        //One or two characters
            '!' => {
                let b = self.match_next('=');
                self.add_token(if b {TokenKind::BangEqual} else {TokenKind::Bang});
            }
            '=' => {
                let b = self.match_next('=');
                self.add_token(if b {TokenKind::EqualEqual} else {TokenKind::Equal});
            }
            '<' => {
                let b = self.match_next('=');
                self.add_token(if b {TokenKind::LessEqual} else {TokenKind::Less});
            }
            '>' => {
                let b = self.match_next('=');
                self.add_token(if b {TokenKind::GreaterEqual} else {TokenKind::Greater});
            }
        //Possibly comment
            '/' => {
                if self.match_next('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenKind::Slash);
                }
            }
        //Number
            '0'..='9' => self.handle_number(),
        //Whitespace
            '\r' | '\t' | ' ' => {},
            '\n' => {
                self.line += 1;
                self.col = 0;
            }
        //String Literals
            '"' => self.handle_string(),
            _ => self.report_err(loc, "Unexpected character".to_string())
        }
    }
}

fn main() -> Result<(), Error> {
    let cl_args = std::env::args();
    let mut args: CommandLineArgs = CommandLineArgs::new();
    for cl_arg in cl_args {
        if cl_arg.ends_with(".lox") {
            args.source_files.push(cl_arg.to_string());
        }
    }

    let file = args.source_files.first().unwrap();
    let mut scanner = Scanner::new();
    scanner.scan(file.clone());

    Ok(())
}

