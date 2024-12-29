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
    Num,
    Flt,
    Str,
    Id,
    Newline,
    Indent,
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
                Num => "<num>",
                Flt => "<flt>",
                Str => "<str>",
                Id => "<id>",
                Newline => "\\n",
                Indent => "<indent>",
                Unk => "<unk>",
                End => "<end>",
            }
        )
    }
}

impl Tok {
    pub fn is_op(self) -> bool {
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

    pub fn precedence(self) -> u8 {
        use Tok::*;
        match self {
            If | Else | While | For | Fn | Ret | Newline | Indent | Unk | End | Id | Str | Num
            | Flt => 0,
            OpenParen | CloseParen => 1,
            Mul | Div | Mod => 2,
            Add | Sub => 3,
            To | ToEq => 4,
            Le | Leq | Ge | Geq => 5,
            EqEq | Neq => 6,
            Eq | AddEq | SubEq | DivEq | MulEq | ModEq => 7,
            Comma => 8,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Token {
    pub ty: Tok,
    pub start: usize,
    pub end: usize,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({})", self.ty, self.len())
    }
}

impl Token {
    pub fn new(ty: Tok, start: usize, end: usize) -> Self {
        Self { ty, start, end }
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    chars: Chars<'a>,
    prev: Token,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            chars: code.chars(),
            prev: Token::new(Tok::Newline, 0, 0),
            pos: 0,
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
        self.pos += self.prev.len();
        let tok = if self.prev.ty == Tok::Newline {
            let mut spaces = 0;
            while let Some(' ') = self.chars.clone().next() {
                spaces += 1;
                self.chars.next();
            }
            Token::new(Tok::Indent, self.pos, self.pos + spaces)
        } else {
            let c = self.chars.next();
            if let Some(c) = c {
                match (c, self.chars.clone().next()) {
                    (' ' | '\r', _) => {
                        self.pos += 1;
                        self.prev = Token::new(Tok::Unk, 0, 0);
                        self.next_tok()
                    }
                    ('\n', _) => Token::new(Tok::Newline, self.pos, self.pos + 1),
                    ('+', Some('=')) => {
                        self.chars.next();
                        Token::new(Tok::AddEq, self.pos, self.pos + 2)
                    }
                    ('-', Some('=')) => {
                        self.chars.next();
                        Token::new(Tok::SubEq, self.pos, self.pos + 2)
                    }
                    ('*', Some('=')) => {
                        self.chars.next();
                        Token::new(Tok::MulEq, self.pos, self.pos + 2)
                    }
                    ('/', Some('=')) => {
                        self.chars.next();
                        Token::new(Tok::DivEq, self.pos, self.pos + 2)
                    }
                    ('%', Some('=')) => {
                        self.chars.next();
                        Token::new(Tok::ModEq, self.pos, self.pos + 2)
                    }
                    ('=', Some('=')) => {
                        self.chars.next();
                        Token::new(Tok::EqEq, self.pos, self.pos + 2)
                    }
                    ('!', Some('=')) => {
                        self.chars.next();
                        Token::new(Tok::Neq, self.pos, self.pos + 2)
                    }
                    ('<', Some('=')) => {
                        self.chars.next();
                        Token::new(Tok::Leq, self.pos, self.pos + 2)
                    }
                    ('>', Some('=')) => {
                        self.chars.next();
                        Token::new(Tok::Geq, self.pos, self.pos + 2)
                    }
                    ('-', Some('>')) => {
                        self.chars.next();
                        Token::new(Tok::To, self.pos, self.pos + 2)
                    }
                    ('=', Some('>')) => {
                        self.chars.next();
                        Token::new(Tok::ToEq, self.pos, self.pos + 2)
                    }
                    ('+', _) => Token::new(Tok::Add, self.pos, self.pos + 1),
                    ('-', _) => Token::new(Tok::Sub, self.pos, self.pos + 1),
                    ('*', _) => Token::new(Tok::Mul, self.pos, self.pos + 1),
                    ('/', _) => Token::new(Tok::Div, self.pos, self.pos + 1),
                    ('%', _) => Token::new(Tok::Mod, self.pos, self.pos + 1),
                    ('=', _) => Token::new(Tok::Eq, self.pos, self.pos + 1),
                    ('<', _) => Token::new(Tok::Le, self.pos, self.pos + 1),
                    ('>', _) => Token::new(Tok::Ge, self.pos, self.pos + 1),
                    ('(', _) => Token::new(Tok::OpenParen, self.pos, self.pos + 1),
                    (')', _) => Token::new(Tok::CloseParen, self.pos, self.pos + 1),
                    (',', _) => Token::new(Tok::Comma, self.pos, self.pos + 1),
                    ('"', _) | ('\'', _) => {
                        let mut i = 0;
                        while let Some(c) = self.chars.next() {
                            if c == '"' || c == '\'' {
                                break;
                            }
                            i += 1;
                        }
                        self.pos += 1; // exclude quote
                        let tok = Token::new(Tok::Str, self.pos, self.pos + i);
                        self.pos += 1; // exclude quote
                        tok
                    }
                    ('#', _) => {
                        while let Some(c) = self.chars.clone().next() {
                            if c == '\n' {
                                break;
                            }
                            self.chars.next();
                            self.pos += 1;
                        }
                        self.pos += 1;
                        self.next_tok()
                    }
                    _ => {
                        if c.is_numeric() {
                            let mut num_len = 1;
                            let mut is_flt = false;
                            while let Some(next_ch) = self.chars.clone().next() {
                                if next_ch.is_numeric() || next_ch == '.' {
                                    if next_ch == '.' {
                                        if is_flt {
                                            panic!("float can only have single dot (.)");
                                        }
                                        is_flt = true;
                                    }
                                    num_len += 1;
                                    self.chars.next();
                                } else {
                                    break;
                                }
                            }
                            if is_flt {
                                Token::new(Tok::Flt, self.pos, self.pos + num_len)
                            } else {
                                Token::new(Tok::Num, self.pos, self.pos + num_len)
                            }
                        } else {
                            let mut id = String::from(c);

                            while let Some(next_ch) = self.chars.clone().next() {
                                if next_ch.is_alphanumeric() || next_ch == '_' {
                                    id.push(next_ch);
                                    self.chars.next();
                                } else {
                                    break;
                                }
                            }

                            match id.as_str() {
                                "if" => Token::new(Tok::If, self.pos, self.pos + id.len()),
                                "else" => Token::new(Tok::Else, self.pos, self.pos + id.len()),
                                "while" => Token::new(Tok::While, self.pos, self.pos + id.len()),
                                "for" => Token::new(Tok::For, self.pos, self.pos + id.len()),
                                "fn" => Token::new(Tok::Fn, self.pos, self.pos + id.len()),
                                "ret" => Token::new(Tok::Ret, self.pos, self.pos + id.len()),
                                str if !str.is_empty() => {
                                    Token::new(Tok::Id, self.pos, self.pos + id.len())
                                }
                                _ => Token::new(Tok::Unk, self.pos, self.pos + id.len()),
                            }
                        }
                    }
                }
            } else {
                Token::new(Tok::End, self.pos, self.pos)
            }
        };
        self.prev = tok;
        tok
    }
}

impl Debug for Lexer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}",
            self.clone().iter().map(|t| t).collect::<Vec<_>>()
        )
    }
}
