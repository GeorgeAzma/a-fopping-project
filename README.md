## FoP project
Simple parser written in rust, for uni project

### How To Run
1. Download Rust from https://www.rust-lang.org/tools/install
2. Enter `cargo run` in terminal

### Language Definition
- similar to python and rust, because of that 
  I'll call it `Marathon` with `.mar` file extension
- interpreted, parsed line by line
- variables are stored as `HashMap(var_name, var_value)` (as in python)
- indentation as scope
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
    else
        1
```
### Language Features
``` python
# Keywords
if, else, while, for, fn, ret

# Operators
+ - / * % ( )
= += -= /= *= %=
== != < > <= >=
-> =>

# Built-in functions
say # prints n args 
max # max of 2 args

# Literals
20 -42
```
### Notes
- `algos.rs` contains all algorithms written in rust for reference
- `test.mar` contains basic test marathon code
### Todo
- finish expressions
    - To `->`
    - ToEq `=>`
    - Paren `()`
- finish statements
    - `for` loop
    - `while` loop
    - `if/else`
- built-in functions
    - `say`
    - `max`
- function calls `my_func()`
    - function arguments `my_func(a, b)`
- operation precendence order
- interpret and run code
    - execute line by line with correct operation order
    - store local variables values in `HashMap<String, StmtIdx>`
- might need other representation like `IR` instead of `AST` for exec
- fix and rewrite parser