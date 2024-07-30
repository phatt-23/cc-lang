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

#[derive(Debug, Clone)]
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
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?}", self.loc, self.kind)
    }
}

