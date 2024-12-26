use std::{
    fmt::{Debug, Display},
    str::Chars,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tok {
    // statements
    If,
    Else,
    While,
    For,
    Fn,
    Ret,
    // range ops
    To,
    ToEq,
    // ops
    // Not,
    Eq,
    Le,
    Ge,
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    // ops (cond, assign)
    Neq,
    EqEq,
    Leq,
    Geq,
    AddEq,
    SubEq,
    DivEq,
    MulEq,
    ModEq,
    // other
    OpenParen,
    CloseParen,
    Comma,
    Quote,
    Comment,
    Space,
    Newline,
    Ident,
    Num,
    Unk,
    End,
}

impl Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Tok::*;
        write!(
            f,
            "{}",
            match self {
                If => "if",
                Else => "else",
                While => "while",
                For => "for",
                Fn => "fn",
                Ret => "ret",
                To => "->",
                ToEq => "=>",
                Eq => "=",
                Le => "<",
                Ge => ">",
                Add => "+",
                Sub => "-",
                Div => "/",
                Mul => "*",
                Mod => "%",
                Neq => "!=",
                EqEq => "==",
                Leq => "<=",
                Geq => ">=",
                AddEq => "+=",
                SubEq => "-=",
                DivEq => "/=",
                MulEq => "*=",
                ModEq => "%=",
                OpenParen => "(",
                CloseParen => ")",
                Comma => ",",
                Quote => "\"",
                Comment => "#",
                Space => " ",
                Newline => "\n",
                Ident => "",
                Num => "",
                Unk => "",
                End => "",
            }
        )
    }
}

impl Tok {
    pub fn is_op(&self) -> bool {
        use Tok::*;
        matches!(
            self,
            To | Add
                | Sub
                | Div
                | Mul
                | Mod
                | Le
                | Ge
                | Eq
                | ToEq
                | AddEq
                | SubEq
                | DivEq
                | MulEq
                | ModEq
                | EqEq
                | Neq
                | Leq
                | Geq
        )
    }
}

#[derive(Clone, Copy)]
pub struct Token {
    pub ty: Tok,
    pub len: usize,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({})", self.ty, self.len)
    }
}

impl Token {
    pub fn new(ty: Tok, len: usize) -> Self {
        Self { ty, len }
    }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            chars: code.chars(),
        }
    }

    pub fn iter(&'a mut self) -> impl Iterator<Item = Token> + 'a {
        std::iter::from_fn(|| {
            let next = self.next_tok();
            if next.ty != Tok::End {
                Some(next)
            } else {
                None
            }
        })
    }

    pub fn next_tok(&mut self) -> Token {
        let c = self.chars.next();
        if c.is_none() {
            return Token::new(Tok::End, 0);
        }
        let c = c.unwrap();
        match (c, self.chars.clone().next()) {
            (' ', _) => {
                let mut spaces = 1;
                while let Some(' ') = self.chars.clone().next() {
                    self.chars.next();
                    spaces += 1;
                }
                Token::new(Tok::Space, spaces)
            }
            ('\r', Some('\n')) | ('\n', Some('\r')) => {
                self.chars.next();
                Token::new(Tok::Newline, 2)
            }
            ('\n', _) => Token::new(Tok::Newline, 1),
            ('+', Some('=')) => {
                self.chars.next();
                Token::new(Tok::AddEq, 2)
            }
            ('-', Some('=')) => {
                self.chars.next();
                Token::new(Tok::SubEq, 2)
            }
            ('*', Some('=')) => {
                self.chars.next();
                Token::new(Tok::MulEq, 2)
            }
            ('/', Some('=')) => {
                self.chars.next();
                Token::new(Tok::DivEq, 2)
            }
            ('%', Some('=')) => {
                self.chars.next();
                Token::new(Tok::ModEq, 2)
            }
            ('=', Some('=')) => {
                self.chars.next();
                Token::new(Tok::EqEq, 2)
            }
            ('!', Some('=')) => {
                self.chars.next();
                Token::new(Tok::Neq, 2)
            }
            ('<', Some('=')) => {
                self.chars.next();
                Token::new(Tok::Leq, 2)
            }
            ('>', Some('=')) => {
                self.chars.next();
                Token::new(Tok::Geq, 2)
            }
            ('-', Some('>')) => {
                self.chars.next();
                Token::new(Tok::To, 2)
            }
            ('=', Some('>')) => {
                self.chars.next();
                Token::new(Tok::ToEq, 2)
            }
            ('+', _) => Token::new(Tok::Add, 1),
            ('-', _) => Token::new(Tok::Sub, 1),
            ('*', _) => Token::new(Tok::Mul, 1),
            ('/', _) => Token::new(Tok::Div, 1),
            ('%', _) => Token::new(Tok::Mod, 1),
            ('=', _) => Token::new(Tok::Eq, 1),
            ('<', _) => Token::new(Tok::Le, 1),
            ('>', _) => Token::new(Tok::Ge, 1),
            ('(', _) => Token::new(Tok::OpenParen, 1),
            (')', _) => Token::new(Tok::CloseParen, 1),
            (',', _) => Token::new(Tok::Comma, 1),
            ('"', _) => Token::new(Tok::Quote, 1),
            ('#', _) => Token::new(Tok::Comment, 1),
            _ => {
                if c.is_numeric() {
                    let mut num_len = 1;
                    while let Some(next_ch) = self.chars.clone().next() {
                        if next_ch.is_numeric() {
                            num_len += 1;
                            self.chars.next();
                        } else {
                            break;
                        }
                    }
                    Token::new(Tok::Num, num_len)
                } else {
                    let mut ident = String::from(c);

                    while let Some(next_ch) = self.chars.clone().next() {
                        if next_ch.is_alphanumeric() || next_ch == '_' {
                            ident.push(next_ch);
                            self.chars.next();
                        } else {
                            break;
                        }
                    }

                    match ident.as_str() {
                        "if" => Token::new(Tok::If, ident.len()),
                        "else" => Token::new(Tok::Else, ident.len()),
                        "while" => Token::new(Tok::While, ident.len()),
                        "for" => Token::new(Tok::For, ident.len()),
                        "fn" => Token::new(Tok::Fn, ident.len()),
                        "ret" => Token::new(Tok::Ret, ident.len()),
                        str if !str.is_empty() => Token::new(Tok::Ident, ident.len()),
                        _ => Token::new(Tok::Unk, ident.len()),
                    }
                }
            }
        }
    }
}

impl Debug for Lexer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}",
            self.clone().iter().map(|t| t.ty).collect::<Vec<_>>()
        )
    }
}
