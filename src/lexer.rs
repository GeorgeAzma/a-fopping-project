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
    Space,
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
                Space => "<space>",
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

    pub fn is_cond(self) -> bool {
        use Tok::*;
        matches!(self, Le | Ge | Eq | EqEq | Neq | Leq | Geq)
    }

    pub fn precedence(self) -> u8 {
        use Tok::*;
        match self {
            If | Else | While | For | Fn | Ret | Newline | Indent | Space | Unk | End | Id
            | Str | Num | Flt => 0,
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

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub ty: Tok,
    pub line: usize,
    pub column: usize,
    pub start: usize,
    pub end: usize,
}

impl Default for Token {
    fn default() -> Self {
        Self {
            ty: Tok::Unk,
            line: 0,
            column: 0,
            start: 0,
            end: 0,
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({})", self.ty, self.len())
    }
}

impl Token {
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    fn next(&mut self, new_tok: Tok, new_len: usize) -> Token {
        let len = self.len();
        self.column += len;
        self.start += len;
        self.end = self.start + new_len;
        if self.ty == Tok::Newline {
            self.column = 0;
            self.line += 1;
        }
        self.ty = new_tok;
        self.clone()
    }

    fn skip(&mut self, chars: usize) {
        self.column += chars;
        self.start += chars;
        self.end += chars;
    }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    chars: Chars<'a>,
    tok: Token,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            chars: code.chars(),
            tok: Token::default(),
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
        let tok = if self.tok.ty == Tok::Newline {
            let mut spaces = 0;
            while let Some(' ') = self.chars.clone().next() {
                spaces += 1;
                self.chars.next();
            }
            self.tok.next(Tok::Indent, spaces)
        } else if self.tok == Default::default() {
            self.tok.next(Tok::Indent, 0)
        } else {
            let c = self.chars.next();
            if let Some(c) = c {
                match (c, self.chars.clone().next()) {
                    (' ' | '\r', _) => {
                        self.tok.next(Tok::Space, 1);
                        self.next_tok()
                    }
                    ('\n', _) => {
                        if self.tok.ty == Tok::Indent {
                            panic!("empty indent");
                        }
                        self.tok.next(Tok::Newline, 1)
                    }
                    ('+', Some('=')) => {
                        self.chars.next();
                        self.tok.next(Tok::AddEq, 2)
                    }
                    ('-', Some('=')) => {
                        self.chars.next();
                        self.tok.next(Tok::SubEq, 2)
                    }
                    ('*', Some('=')) => {
                        self.chars.next();
                        self.tok.next(Tok::MulEq, 2)
                    }
                    ('/', Some('=')) => {
                        self.chars.next();
                        self.tok.next(Tok::DivEq, 2)
                    }
                    ('%', Some('=')) => {
                        self.chars.next();
                        self.tok.next(Tok::ModEq, 2)
                    }
                    ('=', Some('=')) => {
                        self.chars.next();
                        self.tok.next(Tok::EqEq, 2)
                    }
                    ('!', Some('=')) => {
                        self.chars.next();
                        self.tok.next(Tok::Neq, 2)
                    }
                    ('<', Some('=')) => {
                        self.chars.next();
                        self.tok.next(Tok::Leq, 2)
                    }
                    ('>', Some('=')) => {
                        self.chars.next();
                        self.tok.next(Tok::Geq, 2)
                    }
                    ('-', Some('>')) => {
                        self.chars.next();
                        self.tok.next(Tok::To, 2)
                    }
                    ('=', Some('>')) => {
                        self.chars.next();
                        self.tok.next(Tok::ToEq, 2)
                    }
                    ('+', _) => self.tok.next(Tok::Add, 1),
                    ('-', _) => self.tok.next(Tok::Sub, 1),
                    ('*', _) => self.tok.next(Tok::Mul, 1),
                    ('/', _) => self.tok.next(Tok::Div, 1),
                    ('%', _) => self.tok.next(Tok::Mod, 1),
                    ('=', _) => self.tok.next(Tok::Eq, 1),
                    ('<', _) => self.tok.next(Tok::Le, 1),
                    ('>', _) => self.tok.next(Tok::Ge, 1),
                    ('(', _) => self.tok.next(Tok::OpenParen, 1),
                    (')', _) => self.tok.next(Tok::CloseParen, 1),
                    (',', _) => self.tok.next(Tok::Comma, 1),
                    ('"', _) | ('\'', _) => {
                        let mut i = 0;
                        while let Some(c) = self.chars.next() {
                            if c == '"' || c == '\'' {
                                break;
                            }
                            i += 1;
                        }
                        self.tok.skip(1); // exclude quote
                        let tok = self.tok.next(Tok::Str, i);
                        self.tok.skip(1); // exclude quote
                        tok
                    }
                    ('#', _) => {
                        let mut skips = 1;
                        while let Some(c) = self.chars.clone().next() {
                            if c == '\n' {
                                break;
                            }
                            self.chars.next();
                            skips += 1;
                        }
                        self.tok.skip(skips);
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
                                            panic!("float can only have 1 dot (.)");
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
                                self.tok.next(Tok::Flt, num_len)
                            } else {
                                self.tok.next(Tok::Num, num_len)
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
                                "if" => self.tok.next(Tok::If, id.len()),
                                "else" => self.tok.next(Tok::Else, id.len()),
                                "while" => self.tok.next(Tok::While, id.len()),
                                "for" => self.tok.next(Tok::For, id.len()),
                                "fn" => self.tok.next(Tok::Fn, id.len()),
                                "ret" => self.tok.next(Tok::Ret, id.len()),
                                str if !str.is_empty() => self.tok.next(Tok::Id, id.len()),
                                _ => self.tok.next(Tok::Unk, id.len()),
                            }
                        }
                    }
                }
            } else {
                self.tok.next(Tok::End, 0)
            }
        };
        tok
    }
}

impl Debug for Lexer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.clone().iter().collect::<Vec<_>>())
    }
}
