use crate::token::{Token, TokenKind};
use crate::location::Location;

pub struct Lexer {
    source: String,
    tokens: Vec<Token>,
    file: String,
    line: usize,
    col: usize,
    current: usize,
    err_reports: Vec<(Location, String)>,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer {
            tokens: Vec::new(),
            source: String::new(),
            file: String::new(),
            line: 0,
            col: 0,
            current: 0,
            err_reports: Vec::new(),
        }
    }

    pub fn lex(&mut self, filepath: String, source: String) -> Vec<Token> {
        self.file = filepath;
        self.source = source;
        while !self.is_at_end() {
            self.lex_token();
        }
        // println!("[INFO][lexer]");
        // for t in &self.tokens {
        //     println!("  {}", t);
        // }
        for e in &self.err_reports {
            println!("[ERROR][lexer] {} {}", e.0, e.1);
        }
        
        self.add_token(TokenKind::Eof);
        self.tokens.clone()
    }

    //Helpers ----------------------------------------------------
    fn report_err(&mut self, loc: Location, message: String) {
        self.err_reports.push((loc, message));
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.col += 1;
        self.source.chars().nth(self.current - 1).unwrap_or_else(|| {
            let loc = Location::create(self.file.clone(), self.line - 1, self.col);
            self.report_err(loc, "Function advance() failed to read character.".to_string());
            '\0'
        })
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

    //Literals ------------------------------------------------------
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

    fn handle_indentifier(&mut self) {
        let start = self.current - 1;
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        let sub_str = self.source.get(start..self.current).unwrap();
        let kind = match sub_str {
            "class"  => TokenKind::Class,
            "fun"    => TokenKind::Fun,
            "return" => TokenKind::Return,
            "if"     => TokenKind::If,
            "else"   => TokenKind::Else,
            "while"  => TokenKind::While,
            "for"    => TokenKind::For,
            "false"  => TokenKind::False,
            "true"   => TokenKind::True,
            "nil"    => TokenKind::Nil,
            "and"    => TokenKind::And,
            "or"     => TokenKind::Or,
            "print"  => TokenKind::Print,
            "this"   => TokenKind::This,
            "super"  => TokenKind::Super,
            "var"    => TokenKind::Var,
            _        => TokenKind::Identifier(sub_str.to_string()),
        };
        self.add_token(kind);
    }


    fn lex_token(&mut self) {
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
            c if c.is_numeric()  => self.handle_number(),
        //Identifier
            c if c.is_ascii_alphabetic() => self.handle_indentifier(),
        //Whitespace 
        //todo: I dont know if '\0' should be considered whitespace
            '\r' | '\t' | '\0' | ' ' => {},
            '\n' => {
                self.line += 1;
                self.col = 0;
            }
        //String Literals
            '"' => self.handle_string(),
            unexpected_char => self.report_err(loc, format!("{} ({}) Unexpected character", unexpected_char, unexpected_char as i32))
        }
    }
}

