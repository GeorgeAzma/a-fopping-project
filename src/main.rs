mod debug;
mod interner;
mod lexer;
use core::panic;
use std::fmt::Debug;

use debug::backtrace;
use interner::{Intern, Interner};
use lexer::*;

type ExprIdx = usize;
#[derive(Debug, Clone)]
enum Expr {
    Int(i64),
    Ident(Intern),
    Op(ExprIdx, Tok, ExprIdx),
    FnCall(ExprIdx, Vec<ExprIdx>),
    Paren(Vec<ExprIdx>),
}

type StmtIdx = usize;
type BlockIdx = usize;
#[derive(Debug)]
enum Stmt {
    Expr(Expr),
    While(ExprIdx, BlockIdx),
    For(ExprIdx, BlockIdx),
    If(ExprIdx, StmtIdx, Option<StmtIdx>),
    Fn(Intern, Vec<Intern>, BlockIdx),
    Ret(ExprIdx),
    Block(Vec<StmtIdx>),
}

struct Parser<'a> {
    code: &'a str,
    stmts: Vec<Stmt>,
    /// pushed/popped when indent/deindent
    block_stacks: Vec<StmtIdx>,
    lexer: Lexer<'a>,
    interns: Interner,
    prev_tok: Token,
    cur_tok: Token,
    pos: usize,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str) -> Self {
        let mut slf = Self {
            code,
            stmts: vec![Stmt::Block(Vec::new())], // default indent 0 block
            block_stacks: vec![0],                // default indent 0 block index (always in stack)
            lexer: Lexer::new(code),
            interns: Interner::new(),
            prev_tok: Token::new(Tok::Unk, 0),
            cur_tok: Token::new(Tok::Unk, 0),
            pos: 0,
            line: 1,
            column: 1,
        };
        slf.parse();
        slf
    }

    /// adds stmt to current scope
    fn add_stmt(&mut self, stmt: Stmt) -> usize {
        let new_stmt = self.stmts.len();
        self.stmts.push(stmt);
        let cur_scope = self.block_stacks[self.block_stacks.len() - 1];
        if let Stmt::Block(block_stmts) = &mut self.stmts[cur_scope] {
            block_stmts.push(new_stmt);
        } else {
            panic!("expected block, got: {:?}", self.stmts[cur_scope]);
        }
        new_stmt
    }

    fn add_expr(&mut self, expr: Expr) -> usize {
        let new_stmt = self.stmts.len();
        self.stmts.push(Stmt::Expr(expr));
        new_stmt
    }

    fn cur(&self) -> Tok {
        self.cur_tok.ty
    }

    fn skip_spaces(&mut self) {
        while self.cur() == Tok::Space {
            self.next_tok();
        }
    }

    fn skip_comments(&mut self) {
        while self.cur() == Tok::Comment {
            while self.cur() != Tok::Newline {
                self.next_tok();
                if self.cur() == Tok::Newline {
                    self.next_line();
                }
            }
        }
    }

    fn accept_with<F>(&mut self, f: F) -> Option<Tok>
    where
        F: FnOnce(&Tok) -> bool,
    {
        self.skip_comments();
        self.skip_spaces();
        if f(&self.cur()) && self.cur() != Tok::Newline && self.cur() != Tok::End {
            self.next_tok();
            Some(self.prev_tok.ty)
        } else {
            None
        }
    }

    fn accept(&mut self, tok: Tok) -> bool {
        self.accept_with(|t| *t == tok).is_some()
    }

    fn accept_space(&mut self) -> bool {
        self.skip_comments();
        if self.cur() == Tok::Space && self.cur() != Tok::Newline {
            self.next_tok();
            true
        } else {
            false
        }
    }

    fn accept_ident(&mut self) -> Option<Intern> {
        self.accept(Tok::Ident).then(|| {
            let str = &self.code[self.pos - self.prev_tok.len..self.pos];
            self.interns.intern(str)
        })
    }

    fn accept_newline(&mut self) -> bool {
        if self.cur() == Tok::Newline {
            self.next_line();
            true
        } else {
            false
        }
    }

    fn panic(&self, args: std::fmt::Arguments) -> ! {
        panic!(
            "{} [Tok::{:?} {}|{}:{}]",
            args,
            self.cur(),
            self.line,
            self.column,
            self.column + self.cur_tok.len,
        )
    }

    fn expected(&self, tok: Tok) -> ! {
        self.panic(format_args!("expected {tok:?}"))
    }

    // TODO: show line/column numbers of where err occured
    fn _expect(&mut self, tok: Tok) {
        if !self.accept(tok) {
            self.expected(tok);
        }
    }

    fn expect_ident(&mut self) -> Intern {
        if let Some(ident_intern) = self.accept_ident() {
            ident_intern
        } else {
            self.expected(Tok::Ident)
        }
    }

    fn expect_space(&mut self) {
        if !self.accept_space() {
            self.expected(Tok::Space)
        }
    }

    fn expect_newline(&mut self) {
        if !self.accept_newline() {
            self.expected(Tok::Newline)
        }
    }

    fn next_tok(&mut self) -> Tok {
        self.pos += self.cur_tok.len;
        self.column += self.cur_tok.len;
        self.prev_tok = self.cur_tok;
        self.cur_tok = self.lexer.next_tok();
        self.cur_tok.ty
    }

    fn next_line(&mut self) -> Tok {
        self.next_tok();
        self.column = 1;
        self.line += 1;
        self.cur_tok.ty
    }

    fn parse(&mut self) {
        while self.next_tok() != Tok::End {
            self.stmt();
        }
    }

    fn last_indent(&mut self) -> usize {
        self.block_stacks.len() - 1
    }

    fn valid_indent(&mut self, block_spaces: usize) {
        assert_eq!(block_spaces % 4, 0, "block must have multiple of 4 spaces");
        let block_indents = block_spaces / 4;
        let expected_indents = self.last_indent() + 1;
        if block_indents != expected_indents {
            self.panic(format_args!(
                "expected {expected_indents} indents, got {block_indents}"
            ));
        }
    }

    /// block comes after `fn`, `while` etc.
    /// block stmts have same indents
    /// indent is 4 spaces more than parent's indent
    fn block(&mut self) -> StmtIdx {
        let block_spaces = self.cur_tok.len;

        let block_idx = self.stmts.len();
        self.stmts.push(Stmt::Block(vec![]));
        self.block_stacks.push(block_idx);

        while self.cur() != Tok::End {
            if self.column == 1 && self.accept_space() {
                if self.prev_tok.len != block_spaces {
                    break;
                }
                self.stmt();
                while self.accept_newline() {}
            } else if self.column == 1 {
                break;
            }
        }
        self.block_stacks.pop();

        block_idx
    }

    fn expr(&mut self) -> ExprIdx {
        let expr = self.stmt();
        if let Stmt::Expr(_) = self.stmts[expr] {
            expr
        } else {
            self.panic(format_args!("expected Expr"))
        }
    }

    fn stmt(&mut self) -> StmtIdx {
        if self.accept(Tok::Fn) {
            let fn_name = self.expect_ident();
            let mut fn_args = vec![];
            while let Some(arg) = self.accept_ident() {
                fn_args.push(arg);
            }
            self.expect_newline();
            let fn_block = self.block();
            self.add_stmt(Stmt::Fn(fn_name, fn_args, fn_block))
        } else if let Some(ident) = self.accept_ident() {
            let ident = self.add_expr(Expr::Ident(ident));
            if let Some(op) = self.accept_with(Tok::is_op) {
                let rhs = self.expr();
                self.add_expr(Expr::Op(ident, op, rhs))
            } else {
                ident
            }
        } else if self.accept(Tok::Ret) {
            let expr = self.expr();
            self.add_stmt(Stmt::Ret(expr))
        } else {
            StmtIdx::MAX
        }
    }
}

impl Debug for Parser<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn get_expr<'a>(expr: ExprIdx, parser: &'a Parser) -> &'a Expr {
            if let Stmt::Expr(expr) = &parser.stmts[expr] {
                expr
            } else {
                panic!("expected Expr");
            }
        }
        fn write_expr(
            f: &mut std::fmt::Formatter<'_>,
            expr: &Expr,
            parser: &Parser,
        ) -> std::fmt::Result {
            match expr {
                Expr::Int(i) => write!(f, "{i}"),
                Expr::Ident(ident) => write!(f, "{}", &parser.interns[*ident]),
                Expr::Op(lhs, op, rhs) => {
                    write_expr(f, get_expr(*lhs, parser), parser)?;
                    write!(f, "{}", op.to_string())?;
                    write_expr(f, get_expr(*rhs, parser), parser)
                }
                Expr::FnCall(_, _) => todo!(),
                Expr::Paren(_) => todo!(),
            }
        }
        fn write_stmt(
            f: &mut std::fmt::Formatter<'_>,
            stmt: &Stmt,
            parser: &Parser,
            indent: usize,
        ) -> std::fmt::Result {
            let indent_str = " ".repeat(indent * 2);
            write!(f, "\n{indent_str}")?;
            let get_block = |block: StmtIdx| -> &[StmtIdx] {
                if let Stmt::Block(block) = &parser.stmts[block] {
                    block
                } else {
                    panic!("expected Expr");
                }
            };
            match stmt {
                Stmt::Expr(expr) => write_expr(f, expr, parser),
                Stmt::While(expr, block) => {
                    write_expr(f, get_expr(*expr, parser), parser)?;
                    write_block(f, get_block(*block), parser, indent + 1)
                }
                Stmt::For(_, _) => todo!(),
                Stmt::If(_, _, _) => todo!(),
                Stmt::Fn(name, args, block) => {
                    write!(f, "{}", &parser.interns[*name])?;
                    for arg in args {
                        write_expr(f, get_expr(*arg, parser), parser)?;
                    }
                    write_block(f, get_block(*block), parser, indent + 1)
                }
                Stmt::Ret(ret) => {
                    write!(f, "ret ")?;
                    write_expr(f, get_expr(*ret, parser), parser)
                }
                Stmt::Block(stmts) => {
                    for stmt in stmts {
                        write_stmt(f, &parser.stmts[*stmt], parser, indent + 1)?;
                    }
                    Ok(())
                }
            }
        }
        fn write_block(
            f: &mut std::fmt::Formatter<'_>,
            stmts: &[StmtIdx],
            parser: &Parser,
            indent: usize,
        ) -> std::fmt::Result {
            for &stmt_idx in stmts {
                let stmt = &parser.stmts[stmt_idx];
                if let Stmt::Block(ref block_stmts) = stmt {
                    write_block(f, block_stmts, parser, indent + 1)?;
                } else {
                    write_stmt(f, stmt, parser, indent)?;
                }
            }
            Ok(())
        }

        write!(f, "{:?}", self.stmts)?;
        if let Stmt::Block(ref stmts) = self.stmts[0] {
            write_block(f, stmts, self, 0)
        } else {
            Ok(())
        }
    }
}

fn main() {
    // ignore, this is for better error messages
    debug::set_panic_hook();

    let code = include_str!("test.mar");
    let ast = Parser::new(code);

    println!("{ast:?}");
    // println!("{:?}", ast.interns);
    // println!("{:?}", ast.stmts);
    // println!("{:?}", ast.block_stacks);
}
