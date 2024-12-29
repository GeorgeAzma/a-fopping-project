# Marathon `FoP Project`
Simple lexer/parser/interpreter written in rust, for uni project

## How To Run
1. Download Rust from https://www.rust-lang.org/tools/install
2. Enter `cargo run` in terminal

## Language Definition
- similar to python and rust, so  I'll call it `Marathon` with `.mar` file extension
- interpreted, parsed line by line
- indentation as scope
- minimal syntax
``` python
fn my_func n m
    foo = 123
    while foo > 0
        foo -= 1
    # iter from `foo` to 8
    for foo -> 8
        n += 1
    # from 0 to n (inclusive), via index `i`
    for 0 => n i
        say(max(i / 2, max(3 + m, n))) # prints max
    if foo == 0
        0
    else if foo == 1
        1
    else
        2
```
- built-in funcs `say`, `msg`, `max`, `min`
##  Grammar Definition
### Tokens `handled by lexer`
``` py
KW: 'if' | 'else' | 'while' | 'for' | 'fn' | 'ret'
OP : '+'  | '-'  | '/'  | '*'  | '%'
   | ASSIGN | COND | RANGE | PAREN
ASSIGN: '=' | '+=' | '-=' | '/=' | '*=' | '%='
COND: '>'  | '<' | '==' | '>=' | '<=' 
RANGE: '->' | '=>'
PAREN: '('  | ')'
STR: '"' | '\''
COMMA: ','
CMT: '#'
NL: '\n' # newline
IND: (' '{4})+ # 4 space indents (only at line start)
# Identifier (alphanumeric, must start with letter)
ID: [a-zA-Z_][a-zA-Z0-9_]* 
NUM: (-)?[0-9]+
FLT: NUM.[0-9]*
```
### Exprs and Stmts `handled by parser`
``` py
expr: NUM | FLT | ID | str | paren | comma | op | fn_call
stmt: expr NL | ret | block | while | for | if | fn
```
### Helpers
``` py
op: expr OP expr
fn_call: ID '(' (expr comma?)? ')'
paren: '(' expr ')'
comma: (expr',')+
str: STR expr STR 
while: expr block
for: expr RANGE expr (ID)? block # only handles indexed num ranges
if: expr block (else (block | if))? 
fn: ID (ID)* block 
block: (NL)? (IND stmt NL)+ # needs to have appropriate amount of indents
cmt: CMT .* NL 
```