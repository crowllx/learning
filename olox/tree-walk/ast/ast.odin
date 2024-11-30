package ast

import tok "../tokenizer"
import "core:strings"


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
    Function,
    Return_Stmt,
}

Return_Stmt :: struct {
    value: Expr,
}
While_Stmt :: struct {
    condition: Expr,
    body:      ^Stmt,
}

Function :: struct {
    params: []string,
    name:   string,
    body:   Block,
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
    ^Call,
}

Variable :: tok.Token

Call :: struct {
    callee: Expr,
    paren:  tok.Token,
    args:   []Expr,
}

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

copy_expr :: proc(expr: Expr) -> Expr {
    switch var in expr {
    case ^Call:
        new_expr := new(Call)
        args: [dynamic]Expr
        for e in var.args {
            append(&args, copy_expr(e))
        }
        new_expr.args = args[:]
        new_expr.callee = copy_expr(var.callee)
        return new_expr

    case ^Unary:
        new_expr := new(Unary)
        new_expr.operator = var.operator
        new_expr.pos = var.pos
        new_expr.expr = copy_expr(var.expr)
        return new_expr

    case ^Binary:
        new_expr := new(Binary)
        new_expr.operator = var.operator
        new_expr.pos = var.pos
        new_expr.right_expr = copy_expr(var.right_expr)
        new_expr.left_expr = copy_expr(var.left_expr)
        return new_expr


    case ^Grouping:
        new_expr := new(Grouping)
        new_expr.pos = var.pos
        new_expr.expr = copy_expr(var.expr)
        return new_expr

    case ^Variable:
        new_expr := new_clone(tok.copy_token(var^))
        return new_expr

    case ^Assignment:
        new_expr := new(Assignment)
        new_expr.value = copy_expr(var.value)
        new_expr.identifier = copy_expr(var.identifier).(^Variable)
        return new_expr

    case ^Logic_Expr:
        new_expr := new(Logic_Expr)
        new_expr.operator = tok.copy_token(var.operator)
        new_expr.right_expr = copy_expr(var.right_expr)
        new_expr.left_expr = copy_expr(var.left_expr)
        return new_expr

    case ^Literal_Expr:
        new_expr := new(Literal_Expr)
        new_expr.lexeme = strings.clone(var.lexeme)
        if s, ok := var.literal.(string); ok {
            new_expr.literal = strings.clone(s)
        } else {
            new_expr.literal = var.literal
        }
        return new_expr

    }
    return nil
}

copy_stmt :: proc(stmt: ^Stmt) -> Stmt {
    switch var in stmt {
    case Decl:
        new_stmt: Decl
        new_stmt.id = strings.clone(var.id)
        new_stmt.expr = copy_expr(var.expr)
        return new_stmt

    case Block:
        new_stmt: Block
        stmts: [dynamic]Stmt
        for &s in var.stmts {
            append(&stmts, copy_stmt(&s))
        }
        new_stmt.stmts = stmts
        return new_stmt

    case If_Stmt:
        new_stmt: If_Stmt
        new_stmt.then_stmt = new_clone(copy_stmt(var.then_stmt))
        new_stmt.else_stmt = new_clone(copy_stmt(var.else_stmt))
        new_stmt.condition = copy_expr(var.condition)
        return new_stmt

    case Function:
        new_stmt: Function
        params: [dynamic]string
        for p in var.params {
            append(&params, p)
        }

        new_stmt.params = params[:]
        new_stmt.name = strings.clone(var.name)
        block_ptr := var.body
        new_stmt.body = copy_stmt(cast(^Stmt)&block_ptr).(Block)
        return new_stmt

    case Expr_Stmt:
        new_stmt: Expr_Stmt
        new_stmt.type = var.type
        new_stmt.expr = copy_expr(var.expr)
        return new_stmt

    case While_Stmt:
        new_stmt: While_Stmt
        new_stmt.condition = copy_expr(var.condition)
        new_stmt.body = new_clone(copy_stmt(var.body))
        return new_stmt

    case Return_Stmt:
        new_stmt: Return_Stmt
        new_stmt.value = copy_expr(var.value)

        return new_stmt
    }
    return nil
}
