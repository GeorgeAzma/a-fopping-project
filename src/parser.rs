use std::{collections::HashMap, fmt::Debug};

use crate::{Lexer, Tok, Token};

pub type ExprIdx = usize;
#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Flt(f64),
    Str(String),
    Id(String),
    Paren(Option<ExprIdx>),           // (expr)?
    Comma(Vec<ExprIdx>),              // {expr,}+
    Op(ExprIdx, Vec<(Tok, ExprIdx)>), // expr + (op + expr)+
    FnCall(ExprIdx, ExprIdx),         // fn_name + paren/comma
}

pub type StmtIdx = usize;
pub type BlockIdx = usize;
#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Ret(Option<ExprIdx>),                    // ret (expr)?
    Block(Vec<StmtIdx>),                     // (indent + expr + \n)+
    While(ExprIdx, BlockIdx),                // cond_expr + NewIndent + block
    For(ExprIdx, Option<ExprIdx>, BlockIdx), // range_op_expr + idx_id + NewIndent + block
    If(ExprIdx, BlockIdx, Option<StmtIdx>),  // cond_expr + NewIndent + block + (else)?
    Fn(ExprIdx, Vec<ExprIdx>, BlockIdx),     // name_id + arg_ids + NewIndent + block
}

pub struct Parser<'a> {
    code: &'a str,
    pub(crate) stmts: Vec<Stmt>,
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
        slf.block();
        slf
    }

    fn intern_id(&mut self, str: String) -> StmtIdx {
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

    fn tok_flt(&self) -> f64 {
        assert!(self.tok() == Tok::Flt, "only Flt tok has flt");
        let flt = self.tok_str();
        flt.parse()
            .unwrap_or_else(|_| panic!("invalid Flt tok: {flt}"))
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

    fn num(&mut self) -> Option<ExprIdx> {
        if self.tok() == Tok::Num {
            let num = self.tok_num();
            let num = self.add_expr(Expr::Int(num));
            self.next_tok();
            Some(num)
        } else {
            None
        }
    }

    fn flt(&mut self) -> Option<ExprIdx> {
        if self.tok() == Tok::Flt {
            let flt = self.tok_flt();
            let flt = self.add_expr(Expr::Flt(flt));
            self.next_tok();
            Some(flt)
        } else {
            None
        }
    }

    fn id(&mut self) -> Option<ExprIdx> {
        if self.tok() == Tok::Id {
            let id = self.tok_id().to_string();
            let id = self.intern_id(id);
            self.next_tok();
            Some(id)
        } else {
            None
        }
    }

    fn str(&mut self) -> Option<ExprIdx> {
        if self.tok() == Tok::Str {
            let str = self.tok_str().to_string();
            let str = self.add_expr(Expr::Str(str));
            self.next_tok();
            Some(str)
        } else {
            None
        }
    }

    fn op(&mut self, lhs: ExprIdx) -> Option<usize> {
        if let Some(op) = self.skip_if(Tok::is_op) {
            let rhs = self.expr_op(false);
            let op = self.add_expr(Expr::Op(lhs, vec![(op, rhs)]));
            while let Some(op_tok) = self.skip_if(Tok::is_op) {
                let rhs = self.expr_op(false);
                if let Stmt::Expr(Expr::Op(_, rhs_ops)) = &mut self.stmts[op] {
                    rhs_ops.push((op_tok, rhs));
                }
            }
            Some(op)
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

    fn comma(&mut self, lhs: ExprIdx) -> Option<ExprIdx> {
        if self.skip(Tok::Comma) {
            let mut exprs = vec![lhs];
            loop {
                let expr = self.expr();
                exprs.push(expr);
                if !self.skip(Tok::Comma) {
                    break;
                }
            }
            let comma = self.add_expr(Expr::Comma(exprs));
            Some(comma)
        } else {
            None
        }
    }

    fn paren(&mut self) -> Option<ExprIdx> {
        if self.skip(Tok::OpenParen) {
            if self.skip(Tok::CloseParen) {
                Some(self.add_expr(Expr::Paren(None)))
            } else {
                let inner_expr = self.expr();
                if !self.skip(Tok::CloseParen) {
                    expected!(self, ")");
                }
                let paren = self.add_expr(Expr::Paren(Some(inner_expr)));
                Some(paren)
            }
        } else {
            None
        }
    }

    fn expr(&mut self) -> ExprIdx {
        self.expr_op(true)
    }

    fn expr_op(&mut self, op: bool) -> ExprIdx {
        self.str()
            .or_else(|| self.paren())
            .or_else(|| self.num())
            .or_else(|| self.flt())
            .or_else(|| {
                self.id().map(|id| {
                    self.paren()
                        .map(|paren| self.add_expr(Expr::FnCall(id, paren)))
                        .unwrap_or(id)
                })
            })
            .map(|expr| op.then(|| self.op(expr).unwrap_or(expr)).unwrap_or(expr))
            .map(|expr| self.comma(expr).unwrap_or(expr))
            .unwrap_or_else(|| expected!(self, "expr"))
    }

    fn block(&mut self) -> StmtIdx {
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
            let stmt = self.stmt();
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

    fn if_(&mut self) -> StmtIdx {
        let cond = self.expr();
        let block = self.block();
        if self.skip(Tok::Else) {
            if self.skip(Tok::If) {
                let else_if_block = self.if_();
                self.add_stmt(Stmt::If(cond, block, Some(else_if_block)))
            } else {
                let else_block = self.block();
                self.add_stmt(Stmt::If(cond, block, Some(else_block)))
            }
        } else {
            self.add_stmt(Stmt::If(cond, block, None))
        }
    }

    fn ret(&mut self) -> StmtIdx {
        if self.tok() == Tok::Newline {
            self.add_stmt(Stmt::Ret(None))
        } else {
            let ret = self.expr();
            self.add_stmt(Stmt::Ret(Some(ret)))
        }
    }

    fn while_(&mut self) -> StmtIdx {
        let cond = self.expr();
        let block = self.block();
        self.add_stmt(Stmt::While(cond, block))
    }

    fn for_(&mut self) -> StmtIdx {
        let range_op = self.expr();
        let idx = self.id();
        let block = self.block();
        self.add_stmt(Stmt::For(range_op, idx, block))
    }

    fn fn_(&mut self) -> StmtIdx {
        let name = self.id().unwrap_or_else(|| expected!(self, "fn name"));
        let mut args = vec![];
        while let Some(arg) = self.id() {
            args.push(arg);
        }
        let block = self.block();
        self.add_stmt(Stmt::Fn(name, args, block))
    }

    fn stmt(&mut self) -> Option<StmtIdx> {
        while self.skip(Tok::Indent) | self.skip(Tok::Newline) | self.skip(Tok::Else) {}
        (!self.skip(Tok::End)).then(|| {
            (self.skip(Tok::Ret).then(|| self.ret()))
                .or_else(|| self.skip(Tok::While).then(|| self.while_()))
                .or_else(|| self.skip(Tok::For).then(|| self.for_()))
                .or_else(|| self.skip(Tok::If).then(|| self.if_()))
                .or_else(|| self.skip(Tok::Fn).then(|| self.fn_()))
                .unwrap_or_else(|| self.expr())
        })
    }
}

//////////////////////// DEBUG PRINT (IGNORE) ////////////////////////////
impl std::fmt::Debug for Parser<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct Writer<'a, 'b> {
            f: &'a mut std::fmt::Formatter<'b>,
            parser: &'a Parser<'a>,
        }
        impl Writer<'_, '_> {
            fn write_expr_no_idx(&mut self, expr: &Expr) -> std::fmt::Result {
                match expr {
                    Expr::Int(i) => write!(self.f, "{i}"),
                    Expr::Flt(f) => write!(self.f, "{f}"),
                    Expr::Id(id) => write!(self.f, "{id}"),
                    Expr::Op(lhs, rhs_ops) => {
                        self.write_expr(lhs)?;
                        for (op, rhs) in rhs_ops {
                            write!(self.f, " {op} ")?;
                            self.write_expr(rhs)?;
                        }
                        Ok(())
                    }
                    Expr::FnCall(name, paren) => {
                        self.write_expr(name)?;
                        self.write_expr(paren)
                    }
                    Expr::Paren(expr) => {
                        write!(self.f, "(")?;
                        if let Some(expr) = expr {
                            self.write_expr(expr)?;
                        }
                        write!(self.f, ")")
                    }
                    Expr::Str(str) => write!(self.f, "\"{str}\""),
                    Expr::Comma(exprs) => {
                        for expr in exprs {
                            self.write_expr(expr)?;
                            if !std::ptr::eq(expr, &exprs[exprs.len() - 1]) {
                                write!(self.f, ", ")?;
                            }
                        }
                        Ok(())
                    }
                }
            }
            fn write_expr(&mut self, expr: &ExprIdx) -> std::fmt::Result {
                let expr = if let Stmt::Expr(expr) = &self.parser.stmts[*expr] {
                    expr
                } else {
                    panic!("expected expr, got: {:?}", self.parser.stmts[*expr]);
                };
                self.write_expr_no_idx(expr)
            }
            fn write_block(&mut self, block: &StmtIdx, indent: usize) -> std::fmt::Result {
                let stmts = if let Stmt::Block(block) = &self.parser.stmts[*block] {
                    block
                } else {
                    panic!("expected block");
                };
                let indent_str = " ".repeat(indent * 2);
                for stmt_idx in stmts {
                    write!(self.f, "{indent_str}")?;
                    self.write_stmt(stmt_idx, indent)?;
                    if !std::ptr::eq(stmt_idx, &stmts[stmts.len() - 1]) {
                        write!(self.f, "\n")?;
                    }
                }
                Ok(())
            }
            fn write_stmt(&mut self, stmt_idx: &StmtIdx, indent: usize) -> std::fmt::Result {
                let stmt = &self.parser.stmts[*stmt_idx];
                let indent_str = " ".repeat(indent * 2);
                match stmt {
                    Stmt::Expr(expr) => self.write_expr_no_idx(expr),
                    Stmt::While(expr, block) => {
                        write!(self.f, "while ")?;
                        self.write_expr(expr)?;
                        write!(self.f, "\n")?;
                        self.write_block(block, indent + 1)
                    }
                    Stmt::For(expr, idx_expr, block) => {
                        write!(self.f, "for ")?;
                        self.write_expr(expr)?;
                        if let Some(idx) = idx_expr {
                            write!(self.f, " ")?;
                            self.write_expr(idx)?;
                        }
                        write!(self.f, "\n")?;
                        self.write_block(block, indent + 1)
                    }
                    Stmt::If(expr, block, else_expr) => {
                        write!(self.f, "if ")?;
                        self.write_expr(expr)?;
                        write!(self.f, "\n")?;
                        if let Some(else_expr) = else_expr {
                            self.write_block(block, indent + 1)?;
                            if let Stmt::If(_, _, _) = self.parser.stmts[*else_expr] {
                                write!(self.f, "\n{indent_str}else ")?;
                                self.write_stmt(else_expr, indent)
                            } else {
                                write!(self.f, "\n{indent_str}else\n")?;
                                self.write_block(else_expr, indent + 1)
                            }
                        } else {
                            self.write_block(block, indent + 1)
                        }
                    }
                    Stmt::Fn(name, args, block) => {
                        write!(self.f, "fn ")?;
                        self.write_expr(name)?;
                        for arg in args {
                            write!(self.f, " ")?;
                            self.write_expr(arg)?;
                        }
                        write!(self.f, "\n")?;
                        self.write_block(block, indent + 1)
                    }
                    Stmt::Ret(ret) => {
                        if let Some(ret) = ret {
                            write!(self.f, "ret ")?;
                            self.write_expr(ret)
                        } else {
                            write!(self.f, "ret")
                        }
                    }
                    Stmt::Block(_) => self.write_block(stmt_idx, indent + 1),
                }
            }
        }
        writeln!(f, "{:?}", self.stmts)?;
        let mut writer = Writer { f, parser: self };
        writer.write_block(&0, 0)
    }
}
