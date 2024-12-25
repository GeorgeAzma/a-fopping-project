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
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    Eq,
    // assignment ops
    AddEq,
    SubEq,
    DivEq,
    MulEq,
    ModEq,
    // cond ops
    EqEq,
    NotEq,
    Less,
    Greater,
    Leq,
    Geq,
    // other
    OpenParen,
    CloseParen,
    Comma,
    Quote,
    Newline,
    Ident,
    Indent,
    Num,
}

impl Tok {
    pub fn is_stmt(&self) -> bool {
        matches!(
            self,
            Tok::If | Tok::Else | Tok::While | Tok::For | Tok::Fn | Tok::Ret
        )
    }

    pub fn as_op(&self) -> Option<Self> {
        match self {
            Tok::Add
            | Tok::Sub
            | Tok::Div
            | Tok::Mul
            | Tok::Mod
            | Tok::Eq
            | Tok::AddEq
            | Tok::SubEq
            | Tok::DivEq
            | Tok::MulEq
            | Tok::ModEq
            | Tok::EqEq
            | Tok::Greater
            | Tok::Less
            | Tok::Leq
            | Tok::Geq => Some(*self),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub enum TokenData {
    Num(i64),
    Ident(String),
    Indent(u32),
    None,
}

#[derive(Clone)]
pub struct Token {
    pub ty: Tok,
    pub data: TokenData,
    pub line: usize,
    pub start: usize,
    #[allow(unused)]
    pub end: usize,
}

impl Token {
    pub fn num(&self) -> i64 {
        match &self.data {
            TokenData::Num(n) => *n,
            _ => panic!("Token is not a number"),
        }
    }

    pub fn ident(&self) -> &str {
        match &self.data {
            TokenData::Ident(s) => s,
            _ => panic!("Token is not an identifier"),
        }
    }

    pub fn indent(&self) -> u32 {
        match &self.data {
            TokenData::Indent(i) => *i,
            _ => panic!("Token is not an indent"),
        }
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.start)
    }
}

pub struct Lexer;

impl Lexer {
    pub fn tokenize(code: &str) -> Vec<Token> {
        let mut toks: Vec<Token> = Vec::new();
        let lines = code.lines();
        for (line_idx, line) in lines.enumerate() {
            let mut chars = line.chars().peekable();
            let mut ch_idx = 0;
            let mut spaces = 0;
            while let Some(' ') = chars.peek() {
                chars.next();
                spaces += 1;
            }
            if spaces >= 4 {
                toks.push(Token {
                    ty: Tok::Indent,
                    data: TokenData::Indent(spaces / 4),
                    line: line_idx,
                    start: ch_idx,
                    end: ch_idx + spaces as usize / 4 * 4,
                });
            }
            while let Some(ch) = chars.next() {
                let add_tok = |toks: &mut Vec<Token>, ty: Tok, tok_len: usize| {
                    toks.push(Token {
                        ty,
                        data: TokenData::None,
                        line: line_idx,
                        start: ch_idx,
                        end: ch_idx + tok_len,
                    });
                };
                match (ch, chars.peek()) {
                    (' ', _) => continue,
                    ('+', Some('=')) => {
                        chars.next();
                        add_tok(&mut toks, Tok::AddEq, 2);
                    }
                    ('-', Some('=')) => {
                        chars.next();
                        add_tok(&mut toks, Tok::SubEq, 2);
                    }
                    ('*', Some('=')) => {
                        chars.next();
                        add_tok(&mut toks, Tok::MulEq, 2);
                    }
                    ('/', Some('=')) => {
                        chars.next();
                        add_tok(&mut toks, Tok::DivEq, 2);
                    }
                    ('%', Some('=')) => {
                        chars.next();
                        add_tok(&mut toks, Tok::ModEq, 2);
                    }
                    ('=', Some('=')) => {
                        chars.next();
                        add_tok(&mut toks, Tok::EqEq, 2);
                    }
                    ('!', Some('=')) => {
                        chars.next();
                        add_tok(&mut toks, Tok::NotEq, 2);
                    }
                    ('<', Some('=')) => {
                        chars.next();
                        add_tok(&mut toks, Tok::Leq, 2);
                    }
                    ('>', Some('=')) => {
                        chars.next();
                        add_tok(&mut toks, Tok::Geq, 2);
                    }
                    ('-', Some('>')) => {
                        chars.next();
                        add_tok(&mut toks, Tok::To, 2);
                    }
                    ('=', Some('>')) => {
                        chars.next();
                        add_tok(&mut toks, Tok::ToEq, 2);
                    }
                    ('+', _) => add_tok(&mut toks, Tok::Add, 1),
                    ('-', _) => add_tok(&mut toks, Tok::Sub, 1),
                    ('*', _) => add_tok(&mut toks, Tok::Mul, 1),
                    ('/', _) => add_tok(&mut toks, Tok::Div, 1),
                    ('%', _) => add_tok(&mut toks, Tok::Mod, 1),
                    ('=', _) => add_tok(&mut toks, Tok::Eq, 1),
                    ('<', _) => add_tok(&mut toks, Tok::Less, 1),
                    ('>', _) => add_tok(&mut toks, Tok::Greater, 1),
                    ('(', _) => add_tok(&mut toks, Tok::OpenParen, 1),
                    (')', _) => add_tok(&mut toks, Tok::CloseParen, 1),
                    (',', _) => add_tok(&mut toks, Tok::Comma, 1),
                    ('"', _) => add_tok(&mut toks, Tok::Quote, 1),
                    _ => {
                        let mut ident = String::new();
                        ident.push(ch);
                        if ch.is_numeric() {
                            while let Some(&next_ch) = chars.peek() {
                                if next_ch.is_numeric() {
                                    ident.push(next_ch);
                                    chars.next();
                                } else {
                                    break;
                                }
                            }
                            toks.push(Token {
                                ty: Tok::Num,
                                data: TokenData::Num(ident.parse().unwrap()),
                                line: line_idx,
                                start: ch_idx,
                                end: ch_idx + ident.len(),
                            });
                        } else {
                            while let Some(&next_ch) = chars.peek() {
                                if next_ch.is_alphanumeric() || next_ch == '_' {
                                    ident.push(next_ch);
                                    chars.next();
                                } else {
                                    break;
                                }
                            }
                            match ident.as_str() {
                                "if" => add_tok(&mut toks, Tok::If, ident.len()),
                                "else" => add_tok(&mut toks, Tok::Else, ident.len()),
                                "while" => add_tok(&mut toks, Tok::While, ident.len()),
                                "for" => add_tok(&mut toks, Tok::For, ident.len()),
                                "fn" => add_tok(&mut toks, Tok::Fn, ident.len()),
                                "ret" => add_tok(&mut toks, Tok::Ret, ident.len()),
                                _ => {
                                    let ident_len = ident.len();
                                    toks.push(Token {
                                        ty: Tok::Ident,
                                        data: TokenData::Ident(ident),
                                        line: line_idx,
                                        start: ch_idx,
                                        end: ch_idx + ident_len,
                                    });
                                }
                            }
                        }
                    }
                }
                ch_idx += 1;
            }
            toks.push(Token {
                ty: Tok::Newline,
                data: TokenData::None,
                line: line_idx,
                start: ch_idx,
                end: ch_idx,
            });
        }
        toks
    }
}
