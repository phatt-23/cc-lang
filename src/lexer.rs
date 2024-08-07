use crate::token::{Token, TokenKind};
use crate::location::Location;

pub struct Lexer {
    source: String,
    file: String,
    line: usize,
    col: usize,
    current: usize,
    err_reports: Vec<(Location, String)>,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer {
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
        let mut tokens: Vec<Token> = Vec::new();
        while !self.is_at_end() {
            if let Some(tok) = self.lex_token() {
                tokens.push(tok);
            }
        }

        tokens.push(Token::create(TokenKind::Eof, self.create_loc()));
        
        // for t in &tokens {
        //     println!("[INFO][lexer][{}] {:?}", t.loc(), t.kind());
        // }
        
        for e in &self.err_reports {
            println!("[ERROR][lexer] {} {}", e.0, e.1);
        }

        tokens
    }

    //Helpers ----------------------------------------------------
    fn report_err(&mut self, loc: Location, message: String) {
        self.err_reports.push((loc, message));
    }

    fn create_loc(&self) -> Location {
        Location::create(self.file.clone(), self.line, self.col)
    }
    
    fn advance(&mut self) -> char {
        self.current += 1;
        self.col += 1;
        self.source.chars().nth(self.current - 1).unwrap_or('\0')
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

    //Literals ------------------------------------------------------
    fn handle_string(&mut self) -> Token {
        let start = self.current;
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.report_err(self.create_loc(), "Unterminated string".to_string())
        }

        let str = self.source.get(start..self.current).unwrap().to_string();
        self.advance();

        Token::create(TokenKind::String(str), self.create_loc())
    }

    fn handle_number(&mut self) -> Token {
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

        let loc = self.create_loc();
        let sub_str = self.source.get(start..self.current).unwrap().to_string();

        if is_real {
            let number = sub_str.parse::<f64>().unwrap_or_else(|_| {
                self.report_err(loc.clone(), format!("Number failed to parse ({})", sub_str));
                0.0_f64
            });
            Token::create(TokenKind::Double(number), loc)
        } else {
            let number = sub_str.parse::<i32>().unwrap_or_else(|_| {
                self.report_err(loc.clone(), format!("Number failed to parse ({})", sub_str));
                0_i32
            });
            Token::create(TokenKind::Int(number), loc)
        }
    }

    fn handle_indentifier(&mut self) -> Token {
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
        Token::create(kind, self.create_loc())
    }


    fn lex_token(&mut self) -> Option<Token> {
        let loc = self.create_loc();
        let c: char = self.advance();
        match c {
        //Single character
            '(' => Some(Token::create(TokenKind::LeftParen, loc)),
            ')' => Some(Token::create(TokenKind::RightParen, loc)),
            '{' => Some(Token::create(TokenKind::LeftBrace, loc)),
            '}' => Some(Token::create(TokenKind::RightBrace, loc)),
            ',' => Some(Token::create(TokenKind::Comma, loc)),
            '.' => Some(Token::create(TokenKind::Dot, loc)),
            ';' => Some(Token::create(TokenKind::Semicolon, loc)),
            '+' => Some(Token::create(TokenKind::Plus, loc)),
            '-' => Some(Token::create(TokenKind::Minus, loc)),
            '*' => Some(Token::create(TokenKind::Star, loc)),
        //One or two characters
            '!' => {
                let b = self.match_next('=');
                Some(Token::create(if b {TokenKind::BangEqual} else {TokenKind::Bang}, loc))
            }
            '=' => {
                let b = self.match_next('=');
                Some(Token::create(if b {TokenKind::EqualEqual} else {TokenKind::Equal}, loc))
            }
            '<' => {
                let b = self.match_next('=');
                Some(Token::create(if b {TokenKind::LessEqual} else {TokenKind::Less}, loc))
            }
            '>' => {
                let b = self.match_next('=');
                Some(Token::create(if b {TokenKind::GreaterEqual} else {TokenKind::Greater}, loc))
            }
        //Possibly comment
            '/' => {
                if self.match_next('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    None
                } else {
                    Some(Token::create(TokenKind::Slash, loc))
                }
            }
        //Number
            c if c.is_numeric()  => Some(self.handle_number()),
        //Identifier
            c if c.is_ascii_alphabetic() => Some(self.handle_indentifier()),
        //Whitespace 
        //todo: I dont know if '\0' should be considered whitespace
            '\r' | '\t' | '\0' | ' ' => None,
            '\n' => {
                self.line += 1;
                self.col = 0;
                None
            }
        //String Literals
            '"' => Some(self.handle_string()),
            unexpected_char => {
                self.report_err(loc, format!("{} ({}) Unexpected character", unexpected_char, unexpected_char as i32));
                None
            }
        }
    }
}

