- let parser handle operator precedence not interpreter
- fix empty dedent statement causing issues
- recursion
- binary ops
- prefix ops
- neg vs sub op
- `if 5 > 4: 1 elif 2 < 4: 2 else 3`
## Error and Warning reports 
Well formatted with colors/suggestions like in rust
must show line, column start/end indices
- **Lexer**
    - Errors
        - invalid tok
        - unclosed delims
    - Warnings
        - `non_snake_case` vars
        - `NonPascalCase` types
        - useless whitespaces
- **Parser**
    - Errors
        - unexpected tok
        - `ret` outside fn
        - division by zero
        - type mismatch
        - undefined var/fn
        - unused/shadowed var/fn
    - Warnings
        - redundant parens `((x))`
        - empty blocks
        - unreachable code
        - always true condition
- **Interpreter**
    - Errors
        - division by zero
## Future Plan
### Array
``` py
arr = [1, 2]

arr += 3 # add 3 to arr
arr -= 1 # pop arr
arr %= 2 # resize (default/zero init)
arr == arr

# indexing like in python
arr[0], arr[-1], arr[0:-1]

[0] * 3 # [0, 0, 0]
[1, 2] * 2 # [1, 2, 1, 2]
```
### String
``` py
a = ''
a += 'hi'
a += 3.1 # a = "hi3.1"
a -= 1 # pop
a %= 2 # resize
```
### Tuple
``` py
t = (0, '1')
a, b = a
a == t.0 == t[0]
```
### Type
``` py
type Vec3
    x 
    y z

    # parser should gen this automatically if not present
    fn new x y z
        ret Self(x: x, y: y, z: z)
        # ret Vec3(x, y, z)
        # ret (x: x, y: y, z: z)
        # ret (x, y, z)

    # op overloading
    fn + other
        ret Self( # multiline parens
            self.x + other.x, 
            self.y + other.y, 
            self.z + other.z, # allow traling comma
        )

    # automatically static if not using self
    fn max other
        ret Self( # multiline parens
            max(self.x, other.x), 
            max(self.y, other.y), 
            max(self.z, other.z),
        )

v = Vec3(0, 1, 2)
v + v # 0, 2, 4
v.max(v)
max(v, v) # equivalent to above
```
### Formatting
``` py
a = fmt("{} {}", 1, 2) # 1 2
say("{a} {}", 3) # 1 2 3
```
### Assert
``` py
3 != 2 # asserts equality by default
3 == 2, "error 3 != 2" # assert with message
3 == 2, "error {} != {}", 3, 2 # formatted message
```
### Macro
``` py
Todo
```
### Filesystem
``` py
use other_file
```
### Toolchain
``` bash
mar file # run file.mar
mar r # run local src/main.mar
mar rr # release mode run
```
### Other
- compiler with minor optimizations
- compile time vars `constness may be auto determined`
- language server protocol `(lsp)`
- optimization