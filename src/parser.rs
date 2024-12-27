use std::{collections::HashMap, fmt::Debug};

use crate::{Lexer, Tok, Token};

pub type ExprIdx = usize;
#[derive(Debug, Clone)]
pub enum Expr {
    Num(i64),
    Id(String),
    Paren(Vec<ExprIdx>),       // ( expr )
    Op(ExprIdx, Tok, ExprIdx), // expr + op + expr
    FnCall(ExprIdx, ExprIdx),  // fn_name + paren
}

pub type StmtIdx = usize;
pub type BlockIdx = usize;
#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Ret(ExprIdx),                            // ret expr
    Block(Vec<StmtIdx>),                     // (indent + expr + \n)+
    While(ExprIdx, BlockIdx),                // cond_expr + NewIndent + block
    For(ExprIdx, Option<ExprIdx>, BlockIdx), // range_op_expr + idx_id + NewIndent + block
    If(ExprIdx, BlockIdx, Option<StmtIdx>),  // cond_expr + NewIndent + block + (else)?
    Fn(ExprIdx, Vec<ExprIdx>, BlockIdx),     // name_id + arg_ids + NewIndent + block
}

pub struct Parser<'a> {
    code: &'a str,
    stmts: Vec<Stmt>,
    intern_map: HashMap<String, StmtIdx>,
    /// pushed/popped when indent/dedent
    block_stacks: Vec<StmtIdx>,
    lexer: Lexer<'a>,
    cur_tok: Token,
    pos: usize,
    line: usize,
    column: usize,
}

macro_rules! expected {
    ($self:expr, $($args: tt)*) => {
        panic!("expected {} got {} at {}|{}:{}", format_args!($($args)*),
            $self.tok(), $self.line + 1, $self.column + 1, $self.column + 1 + $self.cur_tok.len())
    }
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str) -> Self {
        let mut slf = Self {
            code,
            stmts: vec![],
            intern_map: HashMap::new(),
            block_stacks: vec![],
            lexer: Lexer::new(code),
            cur_tok: Token::new(Tok::Unk, 0, 0),
            pos: 0,
            line: 0,
            column: 0,
        };
        slf.next_tok();
        slf.parse_block();
        slf
    }

    fn intern(&mut self, str: String) -> StmtIdx {
        use std::collections::hash_map::Entry;
        let self_ptr = std::ptr::from_mut(self);
        match self.intern_map.entry(str.clone()) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let ident_stmt_idx = unsafe { &mut *self_ptr }.add_expr(Expr::Id(str));
                entry.insert(ident_stmt_idx);
                ident_stmt_idx
            }
        }
    }

    fn tok(&self) -> Tok {
        self.cur_tok.ty
    }

    fn tok_str(&self) -> &str {
        &self.code[self.cur_tok.start..self.cur_tok.end]
    }

    fn tok_id(&self) -> &str {
        assert!(self.tok() == Tok::Id, "only Id tok has identifier");
        self.tok_str()
    }

    fn tok_num(&self) -> i64 {
        assert!(self.tok() == Tok::Num, "only Num tok has num");
        let num = self.tok_str();
        num.parse()
            .unwrap_or_else(|_| panic!("invalid Num tok: {num}"))
    }

    fn next_tok(&mut self) -> Tok {
        self.pos = self.cur_tok.start;
        let next_tok = self.lexer.next_tok();
        if self.tok() == Tok::Newline {
            self.column = 0;
            self.line += 1;
        } else {
            self.column += next_tok.start - self.pos;
        }
        // println!(
        //     "{:?} -> {:?} \x1b[2m|> {}\x1b[0m",
        //     self.cur_tok,
        //     next_tok,
        //     crate::debug::backtrace(1)
        // );
        self.cur_tok = next_tok;
        self.tok()
    }

    fn skip_if<F: FnOnce(Tok) -> bool>(&mut self, f: F) -> Option<Tok> {
        let tok = self.tok();
        if f(tok) {
            self.next_tok();
            Some(tok)
        } else {
            None
        }
    }

    fn skip(&mut self, tok: Tok) -> bool {
        self.skip_if(|t| t == tok).is_some()
    }

    fn add_stmt_to_block(&mut self, stmt: StmtIdx) {
        let idx = self.block_stacks[self.block_stacks.len() - 1];
        if let Stmt::Block(block_stmts) = &mut self.stmts[idx] {
            block_stmts.push(stmt)
        } else {
            expected!(self, "block")
        }
    }

    fn parse_num(&mut self) -> Option<ExprIdx> {
        if self.tok() == Tok::Num {
            let num = self.tok_num();
            let num = self.add_expr(Expr::Num(num));
            self.next_tok();
            Some(num)
        } else {
            None
        }
    }

    fn parse_id(&mut self) -> Option<ExprIdx> {
        if self.tok() == Tok::Id {
            let id = self.tok_id().to_string();
            let id = self.intern(id);
            self.next_tok();
            Some(id)
        } else {
            None
        }
    }

    fn add_stmt(&mut self, stmt: Stmt) -> StmtIdx {
        self.stmts.push(stmt);
        self.stmts.len() - 1
    }

    fn add_expr(&mut self, expr: Expr) -> ExprIdx {
        self.add_stmt(Stmt::Expr(expr))
    }

    fn parse_paren(&mut self) -> ExprIdx {
        let mut exprs = vec![];
        loop {
            if self.skip(Tok::CloseParen) {
                break;
            }
            let expr = self.parse_expr();
            exprs.push(expr);
            self.skip(Tok::Comma);
        }
        self.add_expr(Expr::Paren(exprs))
    }

    fn parse_expr(&mut self) -> ExprIdx {
        if let Some(num) = self.parse_num() {
            if let Some(op) = self.skip_if(Tok::is_op) {
                let rhs = self.parse_expr();
                self.add_expr(Expr::Op(num, op, rhs))
            } else {
                num
            }
        } else if let Some(id) = self.parse_id() {
            if let Some(op) = self.skip_if(Tok::is_op) {
                let rhs = self.parse_expr();
                self.add_expr(Expr::Op(id, op, rhs))
            } else if self.skip(Tok::OpenParen) {
                let paren = self.parse_paren();
                self.add_expr(Expr::FnCall(id, paren))
            } else {
                id
            }
        } else if self.skip(Tok::OpenParen) {
            self.parse_paren()
        } else {
            expected!(self, "expr")
        }
    }

    fn parse_block(&mut self) -> StmtIdx {
        self.skip(Tok::Newline);
        if self.tok() != Tok::Indent {
            expected!(self, "indent or implicit 0 indent from lexer");
        }

        let block = self.add_stmt(Stmt::Block(vec![]));
        self.block_stacks.push(block);
        let block_indents = self.cur_tok.len();
        while self.tok() == Tok::Indent {
            if self.cur_tok.len() != block_indents {
                break;
            }
            self.next_tok();
            let stmt = self.parse_stmt();
            if let Some(stmt) = stmt {
                self.add_stmt_to_block(stmt);
                while self.skip(Tok::Newline) {
                    if self.tok() != Tok::Indent {
                        expected!(self, "indent");
                    }
                }
            } else {
                break;
            }
        }
        self.block_stacks.pop();
        block
    }

    fn parse_if(&mut self) -> StmtIdx {
        let cond = self.parse_expr();
        let block = self.parse_block();
        if self.skip(Tok::Else) {
            if self.skip(Tok::If) {
                let else_if_block = self.parse_if();
                self.add_stmt(Stmt::If(cond, block, Some(else_if_block)))
            } else {
                let else_block = self.parse_block();
                self.add_stmt(Stmt::If(cond, block, Some(else_block)))
            }
        } else {
            self.add_stmt(Stmt::If(cond, block, None))
        }
    }

    fn parse_ret(&mut self) -> StmtIdx {
        let ret = self.parse_expr();
        self.add_stmt(Stmt::Ret(ret))
    }

    fn parse_while(&mut self) -> StmtIdx {
        let cond = self.parse_expr();
        let block = self.parse_block();
        self.add_stmt(Stmt::While(cond, block))
    }

    fn parse_for(&mut self) -> StmtIdx {
        let range_op = self.parse_expr();
        let idx = self.parse_id();
        let block = self.parse_block();
        self.add_stmt(Stmt::For(range_op, idx, block))
    }

    fn parse_fn(&mut self) -> StmtIdx {
        let name = self
            .parse_id()
            .unwrap_or_else(|| expected!(self, "fn name"));
        let mut args = vec![];
        while let Some(arg) = self.parse_id() {
            args.push(arg);
        }
        let block = self.parse_block();
        self.add_stmt(Stmt::Fn(name, args, block))
    }

    fn parse_stmt(&mut self) -> Option<StmtIdx> {
        if self.skip(Tok::Ret) {
            Some(self.parse_ret())
        } else if self.skip(Tok::While) {
            Some(self.parse_while())
        } else if self.skip(Tok::For) {
            Some(self.parse_for())
        } else if self.skip(Tok::If) {
            Some(self.parse_if())
        } else if self.skip(Tok::Fn) {
            Some(self.parse_fn())
        } else if self.skip(Tok::Indent) {
            None
        } else if self.skip(Tok::Newline) {
            None
        } else if self.skip(Tok::End) {
            None
        } else {
            Some(self.parse_expr())
        }
    }
}

//////////////////////// DEBUG PRINT (IGNORE) ////////////////////////////
impl std::fmt::Debug for Parser<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn write_expr_no_idx(
            f: &mut std::fmt::Formatter<'_>,
            parser: &Parser,
            expr: &Expr,
        ) -> std::fmt::Result {
            match expr {
                Expr::Num(i) => write!(f, "{i}"),
                Expr::Id(id) => write!(f, "{id}"),
                Expr::Op(lhs, op, rhs) => {
                    write_expr(f, parser, lhs)?;
                    write!(f, " {} ", op.to_string())?;
                    write_expr(f, parser, rhs)
                }
                Expr::FnCall(name, paren) => {
                    write_expr(f, parser, name)?;
                    write_expr(f, parser, paren)
                }
                Expr::Paren(exprs) => {
                    write!(f, "(")?;
                    for expr in exprs {
                        write_expr(f, parser, expr)?;
                        if !std::ptr::eq(expr, &exprs[exprs.len() - 1]) {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")
                }
            }
        }
        fn write_expr(
            f: &mut std::fmt::Formatter<'_>,
            parser: &Parser,
            expr: &ExprIdx,
        ) -> std::fmt::Result {
            let expr = if let Stmt::Expr(expr) = &parser.stmts[*expr] {
                expr
            } else {
                panic!("expected expr, got: {:?}", parser.stmts[*expr]);
            };
            write_expr_no_idx(f, parser, expr)
        }
        fn write_block(
            f: &mut std::fmt::Formatter<'_>,
            parser: &Parser,
            block: &StmtIdx,
            indent: usize,
        ) -> std::fmt::Result {
            let stmts = if let Stmt::Block(block) = &parser.stmts[*block] {
                block
            } else {
                panic!("expected block");
            };
            let indent_str = " ".repeat(indent * 2);
            for stmt_idx in stmts {
                write!(f, "{indent_str}")?;
                write_stmt(f, parser, stmt_idx, indent)?;
                if !std::ptr::eq(stmt_idx, &stmts[stmts.len() - 1]) {
                    write!(f, "\n")?;
                }
            }
            Ok(())
        }
        fn write_stmt(
            f: &mut std::fmt::Formatter<'_>,
            parser: &Parser,
            stmt_idx: &StmtIdx,
            indent: usize,
        ) -> std::fmt::Result {
            let stmt = &parser.stmts[*stmt_idx];
            let indent_str = " ".repeat(indent * 2);
            match stmt {
                Stmt::Expr(expr) => write_expr_no_idx(f, parser, expr),
                Stmt::While(expr, block) => {
                    write!(f, "while ")?;
                    write_expr(f, parser, expr)?;
                    write!(f, "\n")?;
                    write_block(f, parser, block, indent + 1)
                }
                Stmt::For(expr, idx_expr, block) => {
                    write!(f, "for ")?;
                    write_expr(f, parser, expr)?;
                    if let Some(idx) = idx_expr {
                        write!(f, " ")?;
                        write_expr(f, parser, idx)?;
                    }
                    write!(f, "\n")?;
                    write_block(f, parser, block, indent + 1)
                }
                Stmt::If(expr, block, else_expr) => {
                    write!(f, "if ")?;
                    write_expr(f, parser, expr)?;
                    write!(f, "\n")?;
                    if let Some(else_expr) = else_expr {
                        write_block(f, parser, block, indent + 1)?;
                        if let Stmt::If(_, _, _) = parser.stmts[*else_expr] {
                            write!(f, "\n{indent_str}else ")?;
                            write_stmt(f, parser, else_expr, indent)
                        } else {
                            write!(f, "\n{indent_str}else\n")?;
                            write_block(f, parser, else_expr, indent + 1)
                        }
                    } else {
                        write_block(f, parser, block, indent + 1)
                    }
                }
                Stmt::Fn(name, args, block) => {
                    write!(f, "fn ")?;
                    write_expr(f, parser, name)?;
                    for arg in args {
                        write!(f, " ")?;
                        write_expr(f, parser, arg)?;
                    }
                    write!(f, "\n")?;
                    write_block(f, parser, block, indent + 1)
                }
                Stmt::Ret(ret) => {
                    write!(f, "ret ")?;
                    write_expr(f, parser, ret)
                }
                Stmt::Block(_) => write_block(f, parser, stmt_idx, indent + 1),
            }
        }
        writeln!(f, "{:?}", self.stmts)?;
        write_block(f, self, &0, 0)
    }
}
