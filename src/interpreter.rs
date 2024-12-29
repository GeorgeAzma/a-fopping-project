use std::{collections::HashMap, fmt::Display};

use crate::{Expr, ExprIdx, Parser, Stmt, StmtIdx, Tok};

#[derive(Clone, Debug)]
enum Value {
    None,
    Int(i64),
    Str(String),
    #[allow(unused)]
    Flt(f64),
    To(i64, i64),
    Paren(Vec<Value>), // fn args (or perhaps tuple)
    Fn(StmtIdx),       // fn ptr
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::None => write!(f, "None"),
            Value::Int(int) => write!(f, "{int}"),
            Value::Str(str) => write!(f, "{str}"),
            Value::Flt(flt) => write!(f, "{flt}"),
            Value::To(start, end) => write!(f, "{start}->{end}"),
            Value::Paren(vals) => {
                write!(f, "(")?;
                for val in vals {
                    write!(f, "{val}")?;
                }
                write!(f, ")")
            }
            Value::Fn(fn_idx) => {
                write!(f, "fn({fn_idx})")
            }
        }
    }
}

pub struct Interpreter<'a> {
    parser: &'a Parser<'a>,
    /// stores local scope var/id vals
    /// stores vals using id's stmt idx in parser (similar to intern strs)
    env_stack: Vec<HashMap<StmtIdx, Value>>,
    ret_tmp: Value,
}

impl<'a> Interpreter<'a> {
    pub fn new(parser: &'a Parser) -> Self {
        Self {
            parser,
            env_stack: Vec::default(),
            ret_tmp: Value::None,
        }
    }

    pub fn interpret(&mut self) {
        // 1st parser stmt is root/main block
        self.stmt(0);
    }

    /// Returns: if fn should early ret
    fn stmt(&mut self, stmt_idx: StmtIdx) -> bool {
        let eval_cond = |slf: &mut Self, cond_idx: StmtIdx| {
            let cond_val = slf.expr(cond_idx);
            if let Value::Int(cond_bool) = cond_val {
                assert!(cond_bool == 0 || cond_bool == 1, "cond didn't eval to bool");
                cond_bool > 0
            } else {
                panic!("expected cond bool, got: {cond_val:?}")
            }
        };
        match &self.parser.stmts[stmt_idx] {
            Stmt::Expr(_) => {
                let expr_val = self.expr(stmt_idx);
                self.add_local(stmt_idx, expr_val)
            }
            Stmt::Ret(ret) => {
                let ret_val = self.expr(*ret);
                self.ret_tmp = ret_val;
                return true;
            }
            Stmt::Block(stmts) => self.block(&stmts),
            Stmt::While(cond, block) => {
                while eval_cond(self, *cond) {
                    self.stmt(*block);
                }
            }
            Stmt::For(to, idx, block) => {
                let to_val = self.expr(*to);
                let (start, end) = if let Value::To(start, end) = to_val {
                    (start, end)
                } else {
                    panic!("");
                };
                for i in start..end {
                    if let Some(idx) = idx {
                        self.add_local(*idx, Value::Int(i));
                    }
                    self.stmt(*block);
                    if let Some(idx) = idx {
                        self.rem_local(*idx);
                    }
                }
            }
            Stmt::If(cond, block, else_block) => {
                if eval_cond(self, *cond) {
                    self.stmt(*block);
                } else if let Some(else_block) = else_block {
                    self.stmt(*else_block);
                }
            }
            Stmt::Fn(name, _, _) => self.add_local(*name, Value::Fn(stmt_idx)),
        }
        false
    }

    fn block(&mut self, stmts: &[StmtIdx]) {
        self.env_stack.push(HashMap::new());
        for &stmt in stmts {
            if self.stmt(stmt) {
                break;
            }
        }
        self.env_stack.pop();
    }

    fn add_local(&mut self, id_expr: ExprIdx, val: Value) {
        let len = self.env_stack.len();
        self.env_stack[len - 1].insert(id_expr, val);
    }

    fn get_or_add_local(&mut self, id_expr: ExprIdx, val: Value) -> Value {
        if let Some(local) = self.find_local(id_expr) {
            local.clone()
        } else {
            self.add_local(id_expr, val.clone());
            val
        }
    }

    fn rem_local(&mut self, id_expr: ExprIdx) {
        let len = self.env_stack.len();
        self.env_stack[len - 1].remove(&id_expr);
    }

    fn find_local(&mut self, id_expr: StmtIdx) -> Option<&mut Value> {
        for scope in self.env_stack.iter_mut().rev() {
            if let Some(value) = scope.get_mut(&id_expr) {
                return Some(value);
            }
        }
        None
    }

    fn local(&mut self, id_expr: StmtIdx) -> &mut Value {
        for scope in self.env_stack.iter_mut().rev() {
            if let Some(value) = scope.get_mut(&id_expr) {
                return value;
            }
        }
        if let Stmt::Expr(Expr::Id(id)) = &self.parser.stmts[id_expr] {
            panic!("undefined var: {id}")
        } else {
            panic!("stmt idx {id_expr} not in parser's stmts")
        }
    }

    fn expr(&mut self, expr_idx: ExprIdx) -> Value {
        let expr = if let Stmt::Expr(expr) = &self.parser.stmts[expr_idx] {
            expr
        } else {
            panic!("failed to eval, expr not found")
        };
        match expr {
            Expr::Int(int) => Value::Int(*int),
            Expr::Flt(flt) => Value::Flt(*flt),
            Expr::Str(str) => Value::Str(str.clone()),
            Expr::Id(_) => self.get_or_add_local(expr_idx, Value::None),
            Expr::Paren(exprs) => {
                let mut expr_vals = vec![];
                for expr in exprs {
                    let expr_val = self.expr(*expr);
                    expr_vals.push(expr_val);
                }
                if expr_vals.len() == 1 {
                    expr_vals[0].clone()
                } else {
                    Value::Paren(expr_vals)
                }
            }
            Expr::Op(lhs_idx, rhs_ops) => {
                // example 1: a + b * c * d + e
                // sort pairs by precedence: [b * c, c * d, a + b, d + e]
                // eval: b = b * c, c = None
                // eval again: c = cd, but c is None so backtrack to b
                // b = b * d = (b * c)d
                // next: a = a + b = a + bcd, b = None
                // next: d = d + e, but d is None so backtrack to c
                // but c is None so backtrack to b
                // but b is None so backtrack to a
                // a = a + e = a + bcd + e
                // example 2: a = b + c * d
                // c = c * d, d = None
                // b = b + c = b + c * d, c = None
                // a = b = b + c * d, b = None
                let mut vals = Vec::with_capacity(rhs_ops.len() + 1);
                vals.push((*lhs_idx, self.expr(*lhs_idx)));
                for (_op, rhs_idx) in rhs_ops.iter() {
                    let rhs_val = self.expr(*rhs_idx);
                    vals.push((*rhs_idx, rhs_val));
                }
                let mut pairs = vec![];
                for (i, (op, _)) in rhs_ops.iter().enumerate() {
                    let lhs_val_idx = i;
                    let rhs_val_idx = i + 1;
                    pairs.push((lhs_val_idx, *op, rhs_val_idx));
                }
                pairs.sort_by_key(|(_, op, _)| op.precedence());
                for (mut lhs_val_idx, op, rhs_val_idx) in pairs {
                    if let Value::None = vals[rhs_val_idx].1 {
                        continue;
                    }
                    // backtrack
                    while let Value::None = vals[lhs_val_idx].1 {
                        if lhs_val_idx == 0 {
                            break;
                        }
                        lhs_val_idx -= 1;
                    }
                    let lhs_idx = vals[lhs_val_idx].0;
                    let res_val = self.op(lhs_idx, &vals[lhs_val_idx].1, op, &vals[rhs_val_idx].1);
                    vals[lhs_val_idx].1 = res_val;
                    vals[rhs_val_idx].1 = Value::None;
                }
                vals[0].1.clone()
            }
            // checks fn name if it's built-in calls it
            // else calls locally defined fn with evaluated paren args
            Expr::FnCall(id, paren) => {
                let paren_args = if let Stmt::Expr(Expr::Paren(args)) = &self.parser.stmts[*paren] {
                    args
                } else {
                    panic!("expected fn paren for fn call");
                };
                if let Stmt::Expr(Expr::Id(fn_name)) = &self.parser.stmts[*id] {
                    // built-in functions
                    match fn_name.as_str() {
                        "say" => {
                            for paren_arg in paren_args {
                                let arg_val = self.expr(*paren_arg);
                                print!("{arg_val}");
                                if !std::ptr::eq(paren_arg, &paren_args[paren_args.len() - 1]) {
                                    print!(" ");
                                } else {
                                    println!("");
                                }
                            }
                            Value::None
                        }
                        "max" => {
                            assert_eq!(
                                paren_args.len(),
                                2,
                                "max takes 2 args, got {}",
                                paren_args.len()
                            );
                            let lhs = self.expr(paren_args[0]);
                            let rhs = self.expr(paren_args[1]);
                            use Value::*;
                            match (lhs, rhs) {
                                (Int(lhs), Int(rhs)) => Int(lhs.max(rhs)),
                                (Flt(lhs), Flt(rhs)) => Flt(lhs.max(rhs)),
                                (Flt(lhs), Int(rhs)) => Flt(lhs.max(rhs as f64)),
                                (Int(lhs), Flt(rhs)) => Flt((lhs as f64).max(rhs)),
                                (lhs, rhs) => panic!("invalid args for max({lhs:?}, {rhs:?})"),
                            }
                        }
                        "min" => {
                            assert_eq!(
                                paren_args.len(),
                                2,
                                "min takes 2 args, got {}",
                                paren_args.len()
                            );
                            let lhs = self.expr(paren_args[0]);
                            let rhs = self.expr(paren_args[1]);
                            use Value::*;
                            match (lhs, rhs) {
                                (Int(lhs), Int(rhs)) => Int(lhs.min(rhs)),
                                (Flt(lhs), Flt(rhs)) => Flt(lhs.min(rhs)),
                                (Flt(lhs), Int(rhs)) => Flt(lhs.min(rhs as f64)),
                                (Int(lhs), Flt(rhs)) => Flt((lhs as f64).min(rhs)),
                                (lhs, rhs) => panic!("invalid args for min({lhs:?}, {rhs:?})"),
                            }
                        }
                        _ => {
                            let fn_idx = *if let Value::Fn(fn_idx) = self.local(*id) {
                                fn_idx
                            } else {
                                if let Stmt::Expr(Expr::Id(fn_name)) = &self.parser.stmts[*id] {
                                    panic!("{fn_name:?} is not callable")
                                } else {
                                    panic!("{:?} is not fn name", self.parser.stmts[*id])
                                }
                            };
                            let fn_ = &self.parser.stmts[fn_idx];
                            if let Stmt::Fn(_id, fn_args, block) = fn_ {
                                for (paren_arg, fn_arg) in paren_args.iter().zip(fn_args) {
                                    let arg_val = self.expr(*paren_arg);
                                    self.add_local(*fn_arg, arg_val);
                                }
                                self.stmt(*block);
                                let mut ret_val = Value::None;
                                std::mem::swap(&mut ret_val, &mut self.ret_tmp);

                                ret_val
                            } else {
                                panic!("{fn_:?} is not callable")
                            }
                        }
                    }
                } else {
                    panic!("expected fn id, got {:?}", &self.parser.stmts[*id])
                }
            }
        }
    }

    fn op(&mut self, lhs_idx: ExprIdx, lhs: &Value, op: Tok, rhs: &Value) -> Value {
        use Value::*;
        let add = |lhs: &Value, rhs: &Value| match (lhs.clone(), rhs.clone()) {
            (Int(lhs), Int(rhs)) => Int(lhs + rhs),
            (Flt(lhs), Flt(rhs)) => Flt(lhs + rhs),
            (Int(lhs), Flt(rhs)) => Flt(lhs as f64 + rhs),
            (Flt(lhs), Int(rhs)) => Flt(lhs + rhs as f64),
            (Str(lhs), Str(rhs)) => Str(lhs + &rhs),
            (Str(lhs), Int(rhs)) => Str(lhs + &rhs.to_string()),
            (Str(lhs), Flt(rhs)) => Str(lhs + &rhs.to_string()),
            (Int(lhs), Str(rhs)) => Str(lhs.to_string() + &rhs),
            (Flt(lhs), Str(rhs)) => Str(lhs.to_string() + &rhs),
            _ => panic!("{lhs:?} + {rhs:?} is invalid"),
        };
        let sub = |lhs: &Value, rhs: &Value| match (lhs.clone(), rhs.clone()) {
            (Int(lhs), Int(rhs)) => Int(lhs - rhs),
            (Flt(lhs), Flt(rhs)) => Flt(lhs - rhs),
            (Int(lhs), Flt(rhs)) => Flt(lhs as f64 - rhs),
            (Flt(lhs), Int(rhs)) => Flt(lhs - rhs as f64),
            (Str(mut lhs), Int(rhs)) => {
                lhs.truncate(lhs.len() - rhs as usize);
                Str(lhs)
            }
            _ => panic!("{lhs:?} - {rhs:?} is invalid"),
        };
        let mul = |lhs: &Value, rhs: &Value| match (lhs.clone(), rhs.clone()) {
            (Int(lhs), Int(rhs)) => Int(lhs * rhs),
            (Flt(lhs), Flt(rhs)) => Flt(lhs * rhs),
            (Int(lhs), Flt(rhs)) => Flt(lhs as f64 * rhs),
            (Flt(lhs), Int(rhs)) => Flt(lhs * rhs as f64),
            (Str(lhs), Int(rhs)) => Str(lhs.repeat(rhs as usize)),
            (Int(lhs), Str(rhs)) => Str(rhs.repeat(lhs as usize)),
            (Str(mut lhs), Flt(rhs)) | (Flt(rhs), Str(mut lhs)) => {
                let lhs_len = (lhs.len() as f64 * rhs).round() as usize;
                if rhs >= 1.0 {
                    lhs = lhs.repeat(rhs.ceil() as usize);
                }
                lhs.truncate(lhs_len);
                Str(lhs)
            }
            _ => panic!("{lhs:?} * {rhs:?} is invalid"),
        };
        let div = |lhs: &Value, rhs: &Value| match (lhs.clone(), rhs.clone()) {
            (Int(lhs), Int(rhs)) => Int(lhs / rhs),
            (Flt(lhs), Flt(rhs)) => Flt(lhs / rhs),
            (Int(lhs), Flt(rhs)) => Flt(lhs as f64 / rhs),
            (Flt(lhs), Int(rhs)) => Flt(lhs / rhs as f64),
            (Str(mut lhs), Int(rhs)) => {
                lhs.truncate(lhs.len() / rhs as usize);
                Str(lhs)
            }
            (Int(lhs), Str(mut rhs)) => {
                rhs.truncate(rhs.len() / lhs as usize);
                Str(rhs)
            }
            (Str(mut lhs), Flt(rhs)) | (Flt(rhs), Str(mut lhs)) => {
                let lhs_len = (lhs.len() as f64 / rhs).round() as usize;
                if rhs >= 1.0 {
                    lhs = lhs.repeat(rhs.ceil() as usize);
                }
                lhs.truncate(lhs_len);
                Str(lhs)
            }
            _ => panic!("{lhs:?} / {rhs:?} is invalid"),
        };
        let m0d = |lhs: &Value, rhs: &Value| match (lhs.clone(), rhs.clone()) {
            (Int(lhs), Int(rhs)) => Int(lhs % rhs),
            (Flt(lhs), Flt(rhs)) => Flt(lhs % rhs),
            (Int(lhs), Flt(rhs)) => Flt(lhs as f64 % rhs),
            (Flt(lhs), Int(rhs)) => Flt(lhs % rhs as f64),
            (Str(mut lhs), Int(rhs)) => {
                lhs.truncate(rhs as usize);
                Str(lhs)
            }
            (Int(lhs), Str(mut rhs)) => {
                rhs.truncate(lhs as usize);
                Str(rhs)
            }
            _ => panic!("{lhs:?} / {rhs:?} is invalid"),
        };
        let to = |lhs: &Value, rhs: &Value| match (lhs.clone(), rhs.clone()) {
            (Int(lhs), Int(rhs)) => To(lhs, rhs),
            _ => panic!("{lhs:?} -> {rhs:?} is invalid"),
        };
        let to_eq = |lhs: &Value, rhs: &Value| match (lhs.clone(), rhs.clone()) {
            (Int(lhs), Int(rhs)) => To(lhs, rhs + rhs.signum()),
            _ => panic!("{lhs:?} => {rhs:?} is invalid"),
        };
        let le = |lhs: &Value, rhs: &Value| -> Value {
            match (lhs.clone(), rhs.clone()) {
                (Int(lhs), Int(rhs)) => Int((lhs < rhs) as i64),
                (Flt(lhs), Flt(rhs)) => Int((lhs < rhs) as i64),
                (Int(lhs), Flt(rhs)) => Int(((lhs as f64) < rhs) as i64),
                (Flt(lhs), Int(rhs)) => Int((lhs < (rhs as f64)) as i64),
                (Str(lhs), Str(rhs)) => Int((lhs < rhs) as i64),
                _ => panic!("{lhs:?} < {rhs:?} is invalid"),
            }
        };
        let leq = |lhs: &Value, rhs: &Value| -> Value {
            match (lhs.clone(), rhs.clone()) {
                (Int(lhs), Int(rhs)) => Int((lhs <= rhs) as i64),
                (Flt(lhs), Flt(rhs)) => Int((lhs <= rhs) as i64),
                (Int(lhs), Flt(rhs)) => Int(((lhs as f64) <= rhs) as i64),
                (Flt(lhs), Int(rhs)) => Int((lhs <= (rhs as f64)) as i64),
                (Str(lhs), Str(rhs)) => Int((lhs <= rhs) as i64),
                _ => panic!("{lhs:?} <= {rhs:?} is invalid"),
            }
        };
        let ge = |lhs: &Value, rhs: &Value| -> Value {
            match (lhs.clone(), rhs.clone()) {
                (Int(lhs), Int(rhs)) => Int((lhs > rhs) as i64),
                (Flt(lhs), Flt(rhs)) => Int((lhs > rhs) as i64),
                (Int(lhs), Flt(rhs)) => Int(((lhs as f64) > rhs) as i64),
                (Flt(lhs), Int(rhs)) => Int((lhs > (rhs as f64)) as i64),
                (Str(lhs), Str(rhs)) => Int((lhs > rhs) as i64),
                _ => panic!("{lhs:?} > {rhs:?} is invalid"),
            }
        };
        let geq = |lhs: &Value, rhs: &Value| -> Value {
            match (lhs.clone(), rhs.clone()) {
                (Int(lhs), Int(rhs)) => Int((lhs >= rhs) as i64),
                (Flt(lhs), Flt(rhs)) => Int((lhs >= rhs) as i64),
                (Int(lhs), Flt(rhs)) => Int(((lhs as f64) >= rhs) as i64),
                (Flt(lhs), Int(rhs)) => Int((lhs >= (rhs as f64)) as i64),
                (Str(lhs), Str(rhs)) => Int((lhs >= rhs) as i64),
                _ => panic!("{lhs:?} >= {rhs:?} is invalid"),
            }
        };
        let eq_eq = |lhs: &Value, rhs: &Value| -> Value {
            match (lhs.clone(), rhs.clone()) {
                (Int(lhs), Int(rhs)) => Int((lhs == rhs) as i64),
                (Flt(lhs), Flt(rhs)) => Int((lhs == rhs) as i64),
                (Int(lhs), Flt(rhs)) => Int(((lhs as f64) == rhs) as i64),
                (Flt(lhs), Int(rhs)) => Int((lhs == (rhs as f64)) as i64),
                (Str(lhs), Str(rhs)) => Int((lhs == rhs) as i64),
                _ => panic!("{lhs:?} == {rhs:?} is invalid"),
            }
        };
        let neq = |lhs: &Value, rhs: &Value| -> Value {
            match (lhs.clone(), rhs.clone()) {
                (Int(lhs), Int(rhs)) => Int((lhs != rhs) as i64),
                (Flt(lhs), Flt(rhs)) => Int((lhs != rhs) as i64),
                (Int(lhs), Flt(rhs)) => Int(((lhs as f64) != rhs) as i64),
                (Flt(lhs), Int(rhs)) => Int((lhs != (rhs as f64)) as i64),
                (Str(lhs), Str(rhs)) => Int((lhs != rhs) as i64),
                _ => panic!("{lhs:?} != {rhs:?} is invalid"),
            }
        };
        {
            macro_rules! assign {
                ($self: expr, $lhs_idx: ident, $lhs: ident, $rhs: ident, $fn: ident) => {{
                    let res = $fn(&$lhs, &$rhs);
                    *self.local($lhs_idx) = res.clone();
                    res.clone()
                }};
            }

            use Tok::*;
            match op {
                If | Else | While | For | Fn | Ret | OpenParen | CloseParen | Comma | Num | Flt
                | Str | Id | Newline | Indent | Unk | End => {
                    panic!("invalid op: {op}")
                }
                To => to(&lhs, &rhs),
                ToEq => to_eq(&lhs, &rhs),
                Eq => {
                    *self.local(lhs_idx) = rhs.clone();
                    rhs.clone()
                }
                Le => le(&lhs, &rhs),
                Ge => ge(&lhs, &rhs),
                Add => add(&lhs, &rhs),
                Sub => sub(&lhs, &rhs),
                Div => div(&lhs, &rhs),
                Mul => mul(&lhs, &rhs),
                Mod => m0d(&lhs, &rhs),
                Neq => neq(&lhs, &rhs),
                EqEq => eq_eq(&lhs, &rhs),
                Leq => leq(&lhs, &rhs),
                Geq => geq(&lhs, &rhs),
                AddEq => {
                    assign!(self, lhs_idx, lhs, rhs, add)
                }
                SubEq => {
                    assign!(self, lhs_idx, lhs, rhs, sub)
                }
                DivEq => {
                    assign!(self, lhs_idx, lhs, rhs, div)
                }
                MulEq => {
                    assign!(self, lhs_idx, lhs, rhs, mul)
                }
                ModEq => {
                    assign!(self, lhs_idx, lhs, rhs, m0d)
                }
            }
        }
    }
}
