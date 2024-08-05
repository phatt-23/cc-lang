use crate::location::Location;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum TokenKind {
    // Single
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    // One or two characters
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    // Literals
    Identifier(String), String(String), Int(i32), Double(f64),
    // Keyword
    Class, Fun, Return,
    If, Else, While, For, 
    False, True, Nil, 
    And, Or, 
    Print,
    This, Super,
    Var,

    Eof
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    loc: Location,
}

impl Token {
    pub fn create(kind: TokenKind, loc: Location) -> Self {
        Token { kind, loc }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn loc(&self) -> &Location {
        &self.loc
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?}", self.loc, self.kind)
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Minus                => write!(f, "-"),
            TokenKind::Plus                 => write!(f, "+"),
            TokenKind::Slash                => write!(f, "/"),
            TokenKind::Star                 => write!(f, "*"),
            TokenKind::Bang                 => write!(f, "!"),
            TokenKind::BangEqual            => write!(f, "!="),
            TokenKind::Equal                => write!(f, "="),
            TokenKind::EqualEqual           => write!(f, "=="),
            TokenKind::Greater              => write!(f, ">"),
            TokenKind::GreaterEqual         => write!(f, ">="),
            TokenKind::Less                 => write!(f, "<"),
            TokenKind::LessEqual            => write!(f, "<="),
            TokenKind::LeftParen            => write!(f, "("),
            TokenKind::RightParen           => write!(f, ")"), 
            TokenKind::LeftBrace            => write!(f, "{{"),
            TokenKind::RightBrace           => write!(f, "}}"),
            TokenKind::Comma                => write!(f, ","),
            TokenKind::Dot                  => write!(f, "."),
            TokenKind::Semicolon            => write!(f, ";"),
            TokenKind::Identifier(ident)    => write!(f, "{}", ident),
            TokenKind::String(string)       => write!(f, "\"{}\"", string),
            TokenKind::Int(int)             => write!(f, "{}", int),
            TokenKind::Double(double)       => write!(f, "{}", double),
            TokenKind::Class                => write!(f, "class"),
            TokenKind::Fun                  => write!(f, "fun"),
            TokenKind::Return               => write!(f, "return"),
            TokenKind::If                   => write!(f, "if"),
            TokenKind::Else                 => write!(f, "else"),
            TokenKind::While                => write!(f, "while"),
            TokenKind::For                  => write!(f, "for"),
            TokenKind::False                => write!(f, "false"),
            TokenKind::True                 => write!(f, "true"),
            TokenKind::Nil                  => write!(f, "nil"),
            TokenKind::And                  => write!(f, "and"),
            TokenKind::Or                   => write!(f, "or"),
            TokenKind::Print                => write!(f, "print"),
            TokenKind::This                 => write!(f, "this"),
            TokenKind::Super                => write!(f, "super"),
            TokenKind::Var                  => write!(f, "var"),
            TokenKind::Eof                  => write!(f, "EOF"),
        }
    }
}

