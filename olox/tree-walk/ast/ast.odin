package ast

import tok "../tokenizer"


Node :: struct {
    pos: int,
}


Stmt_Type :: enum {
    PRINT_STMT,
    EXPR_STMT,
}
Stmt_Base :: struct {
    expr: Expr,
}

Stmt :: union {
    If_Stmt,
    While_Stmt,
    Expr_Stmt,
    Decl,
    Block,
}

While_Stmt :: struct {
    condition: Expr,
    body:      ^Stmt,
}

If_Stmt :: struct {
    condition: Expr,
    then_stmt: ^Stmt,
    else_stmt: ^Stmt,
}

Expr_Stmt :: struct {
    using base: Stmt_Base,
    type:       Stmt_Type,
}

Decl :: struct {
    using base: Stmt_Base,
    id:         string,
}

Block :: struct {
    stmts: [dynamic]Stmt,
}


// Expressions
Expr :: union {
    ^Unary,
    ^Binary,
    ^Literal_Expr,
    ^Grouping,
    ^Assignment,
    ^Variable,
    ^Logic_Expr,
}

Variable :: tok.Token


Logic_Expr :: struct {
    operator:   tok.Token,
    left_expr:  Expr,
    right_expr: Expr,
}
Assignment :: struct {
    identifier: ^Variable,
    value:      Expr,
}

Literal_Expr :: struct {
    using node: Node,
    literal:    tok.Literal,
    lexeme:     string,
}

Grouping :: struct {
    using node: Node,
    expr:       Expr,
}

Unary :: struct {
    using node: Node,
    operator:   tok.Token,
    expr:       Expr,
}

Binary :: struct {
    using node: Node,
    left_expr:  Expr,
    operator:   tok.Token,
    right_expr: Expr,
}
