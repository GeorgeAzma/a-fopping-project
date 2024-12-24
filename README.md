## FoP project
Simple parser written in rust, for uni project

### How To Run
1. Download Rust from https://www.rust-lang.org/tools/install
2. Enter `cargo run` in terminal

### Language Definition
- similar to python and rust
- interpreted, parsed line by line
- variables are stored as `HashMap(var_name, var_value)` (as in python)
- indentation as scope
``` c
fn my_func()
    foo = 123
    while foo > 0
        foo -= 1
    
    sum = 0
    # iter from `foo` to 8, print `i / 2`
    for foo -> 8
        sum += 1
    # from 0 to sum (inclusive), via index `i` 
    for 0 => sum i
        say(i / 2)

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
+= -= /= *= %=
== != < > <= >=
-> =>

# Built-in functions
say, max
```
### Notes
- `algos.rs` file contains all algorithms written in rust
used as reference