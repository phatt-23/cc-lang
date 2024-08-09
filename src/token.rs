use std::hash::{Hash, Hasher};
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

impl Eq for TokenKind {
    fn assert_receiver_is_total_eq(&self) {}
}

impl Hash for TokenKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::LeftParen => 0_u8.hash(state),
            Self::RightParen => 1_u8.hash(state),
            Self::LeftBrace => 2_u8.hash(state),
            Self::RightBrace => 3_u8.hash(state),
            Self::Comma => 4_u8.hash(state),
            Self::Dot => 5_u8.hash(state),
            Self::Minus => 6_u8.hash(state),
            Self::Plus => 7_u8.hash(state),
            Self::Semicolon => 8_u8.hash(state),
            Self::Slash => 9_u8.hash(state),
            Self::Star => 10_u8.hash(state),
            Self::Bang => 11_u8.hash(state),
            Self::BangEqual => 12_u8.hash(state),
            Self::Equal => 13_u8.hash(state),
            Self::EqualEqual => 14_u8.hash(state),
            Self::Greater => 15_u8.hash(state),
            Self::GreaterEqual => 16_u8.hash(state),
            Self::Less => 17_u8.hash(state),
            Self::LessEqual => 18_u8.hash(state),
            Self::Identifier(s) => {
                19_u8.hash(state);
                s.hash(state);
            }
            Self::String(s) => {
                20_u8.hash(state);
                s.hash(state);
            }
            Self::Int(i) => {
                21_u8.hash(state);
                i.hash(state);
            }
            Self::Double(f) => {
                22_u8.hash(state);
                f.to_bits().hash(state);
            }
            Self::Class => 23_u8.hash(state),
            Self::Fun => 24_u8.hash(state),
            Self::Return => 25_u8.hash(state),
            Self::If => 26_u8.hash(state),
            Self::Else => 27_u8.hash(state),
            Self::While => 28_u8.hash(state),
            Self::For => 29_u8.hash(state),
            Self::False => 30_u8.hash(state),
            Self::True => 31_u8.hash(state),
            Self::Nil => 32_u8.hash(state),
            Self::And => 33_u8.hash(state),
            Self::Or => 34_u8.hash(state),
            Self::Print => 35_u8.hash(state),
            Self::This => 36_u8.hash(state),
            Self::Super => 37_u8.hash(state),
            Self::Var => 38_u8.hash(state),
            Self::Eof => 39_u8.hash(state),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    loc: Location,
}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state)
    }
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

