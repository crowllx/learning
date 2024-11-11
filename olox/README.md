## olox

following 
the book [crafting interpreters](https://craftinginterpreters.com) but using odin instead of java/c


### features of the language

 - automatic memory management
 - dynamically typed
 - datatypes:
     * booleans
     * numbers (double precision float)
     * strings
     * nil 
 - equality can be tested on differt types `123 == "123" // false`
 - operators:
     * not `!`
     * and `and` 
     * or `or`
 - variables:
     * `var keyword declares`
     * value defaults to nil if not initialized
 - control flow
    * c style for loops
    * while loops
    * if/else
 - functions
    * `fun` to declare a function
    * defaults to returning nil if no return keyword 
    * first-class functions, closures
 - classes
    * can freely add fields onto objects
    * inheritance `class example < something`
    * standard library only consists of print and close


```python
def something():
    return 1 + 2

```


### grammar 

```
expression  -> literal
                | unary
                | binary
                | grouping ;
            
literal     -> NUMBER | STRING | 'true' | 'false' | 'nil';
grouping    -> "(" expression ")" ;
unary       -> ( "-" | "!") expression ;
binary      -> expression operator expression ;
operator    -> "==" | "!-" | "<" |"<=" | ">" | ">=" | "=" |"-" | "*" | "/" 

```

### precedence

```
expression  -> equality ;
equality    -> comparison ( ( "!=" | "==" ) comparison )* ;
comparison  -> term ( ( ">" | ">=" | "<" | <= ) term )* ;
term        -> factor ( ( "-" | "+" ) factor )* ;
factor      -> unary ( ( "\" | "*") unary )*;
unary       -> ( "!" | "-") unary | primary;
primary     -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")";
```

