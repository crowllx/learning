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

### grammar 


```
program     -> declaration* EOF ;
declaration -> func_declaration | varDecl | statement ;

func_declaration -> "fun" function;
function    -> IDENTIFIER "(" parameters? ")" block ;
varDecl     -> "var" IDENTIFIER ( "=" expression )? ";" ;

statement   -> expr_stmt |
               for_stmt | 
               if_stmt |
               print_stmt |
               return_stmt |
               while_stmt |
               block ;


return_stmt -> "return" expression? ";" ;
for_stmt    -> "for" "(" ( varDevl | exprStmt | ";" )
                    expression? ";"
                    expression? ")" statement ;
                    
while_stmt  -> "while" "(" expression ")" statement ;
if_stmt     -> "if" "(" expression ")" statement ("else" statement)? ;
block ->    -> "{" declaration* "}" ;
expr_stmt   -> expression ";" ;
print_stmt  -> "print" expression ";" ;

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
expression  -> assignment;
assignment  -> IDENTIFIER "=" assignment | equality ;
equality    -> comparison ( ( "!=" | "==" ) comparison )* ;
comparison  -> term ( ( ">" | ">=" | "<" | <= ) term )* ;
term        -> factor ( ( "-" | "+" ) factor )* ;
factor      -> unary ( ( "\" | "*") unary )*;
unary       -> ( "!" | "-") unary | primary;
arguments   -> expression ( "," expression)* ;
primary     -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
                | IDENTIFIER;
```
