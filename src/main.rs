mod lexer;
use lexer::*;

type ExprIdx = usize;
#[derive(Debug)]
enum Expr {
    Int(i64),
    Ident(String),
    Op(ExprIdx, Tok, ExprIdx),
    FnCall(String, Vec<String>),
    Paren(Vec<ExprIdx>),
}

type StmtIdx = usize;
#[derive(Debug)]
enum Stmt {
    While(ExprIdx, StmtIdx),
    For(ExprIdx, StmtIdx),
    If(ExprIdx, StmtIdx, Option<StmtIdx>),
    Fn(String, Vec<String>, StmtIdx),
    Ret(ExprIdx),
}

struct Parser {
    exprs: Vec<Expr>,
    stmts: Vec<Stmt>,
    toks: Vec<Token>,
    cur_tok: usize,
}

impl Parser {
    pub fn new(toks: &[Token]) -> Self {
        let mut slf = Self {
            exprs: Default::default(),
            stmts: Default::default(),
            toks: toks.to_vec(),
            cur_tok: 0,
        };
        println!("{:?}", size_of::<Tok>());
        slf.parse();
        slf
    }

    fn add_expr(&mut self, expr: Expr) -> usize {
        self.exprs.push(expr);
        self.exprs.len() - 1
    }

    fn add_stmt(&mut self, stmt: Stmt) -> usize {
        self.stmts.push(stmt);
        self.stmts.len() - 1
    }

    fn cur(&mut self) -> Tok {
        self.toks[self.cur_tok].ty
    }

    fn accept(&mut self, tok: Tok) -> bool {
        if tok == self.toks[self.cur_tok].ty {
            self.next().is_some()
        } else {
            false
        }
    }

    fn next(&mut self) -> Option<Tok> {
        self.cur_tok += 1;
        if self.cur_tok >= self.toks.len() {
            None
        } else {
            Some(self.toks[self.cur_tok].ty)
        }
    }

    fn parse(&mut self) {
        while self.cur_tok < self.toks.len() {
            if self.cur() == Tok::Newline {
                self.next();
                continue;
            } else if self.cur().is_stmt() {
                self.stmt();
            } else {
                self.expr();
            }
        }
    }

    fn stmt(&mut self) -> usize {
        if self.accept(Tok::Ret) {
            let expr = self.expr();
            self.add_stmt(Stmt::Ret(expr))
        } else {
            panic!("expected stmt: {:?}", self.cur());
        }
    }

    fn expr(&mut self) -> usize {
        if let Tok::Ident = self.cur() {
            let ident = self.toks[self.cur_tok].ident().to_string();
            let expr_a = self.add_expr(Expr::Ident(ident));
            if let Some(next) = self.next() {
                if let Some(op) = next.as_op() {
                    self.next();
                    let expr_b = self.expr();
                    self.add_expr(Expr::Op(expr_a, op, expr_b));
                }
            }
        } else if let Tok::Num = self.cur() {
            let num = self.toks[self.cur_tok].num();
            self.next();
            self.add_expr(Expr::Int(num));
        } else {
            panic!("expected expr: {:?}", self.cur());
        }
        self.exprs.len() - 1
    }
}

fn main() {
    let code = include_str!("test.mar");
    let toks = Lexer::tokenize(code);
    let ast = Parser::new(&toks);
    println!("{:?}", ast.exprs);
    println!("{:?}", ast.stmts);
}
