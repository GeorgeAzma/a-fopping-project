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
        say max i / 2, (max 3 + m, n)  # 1. syntax variant (not decided yet)
        say(max(i / 2, max(3 + m, n))) # 2. syntax variant (not decided yet)

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
- `algos.rs` file contains all algorithms written in rust
used as reference