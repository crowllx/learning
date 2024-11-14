package ast

import tok "../tokenizer"


Node :: struct {
    pos: int,
}

StmtType :: enum {
    PRINT_STMT,
    EXPR_STMT,
    DECL,
}

Stmt_base :: struct {
    type: StmtType,
    expr: Expr,
}

Stmt :: struct {
    using base: Stmt_base,
    id:         string,
}


// Expressions
Expr :: union {
    ^Unary,
    ^Binary,
    ^LiteralExpr,
    ^Grouping,
}

LiteralExpr :: struct {
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
