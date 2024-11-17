package ast

import tok "../tokenizer"


Node :: struct {
    pos: int,
}


StmtType :: enum {
    PRINT_STMT,
    EXPR_STMT,
}
StmtBase :: struct {
    expr: Expr,
}

Stmt :: union {
    Expr_Stmt,
    Decl,
    Block,
}
Expr_Stmt :: struct {
    using base: StmtBase,
    type:  StmtType,
}

Decl :: struct {
    using base: StmtBase,
    id:         string,
}

Block :: struct {
    stmts: [dynamic]Stmt,
}


// Expressions
Expr :: union {
    ^Unary,
    ^Binary,
    ^LiteralExpr,
    ^Grouping,
    ^Assignment,
    Variable,
}

Variable :: tok.Token

Assignment :: struct {
    identifier: Variable,
    value:      Expr,
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
