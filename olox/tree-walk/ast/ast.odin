package ast

import tok "../tokenizer"
import "core:fmt"
import "core:strings"


Node :: struct {
    pos: int,
}

// Expressions

Expression :: union {
    ^Unary,
    ^Binary,
    ^Literal,
    ^Grouping,
}

Literal :: struct {
    using node: Node,
    value:      tok.Token,
}

Grouping :: struct {
    using node: Node,
    expr:       Expression,
}

Unary :: struct {
    using node: Node,
    operator:   tok.Token,
    expr:       Expression,
}

Binary :: struct {
    using node: Node,
    left_expr:  Expression,
    operator:   tok.Token,
    right_expr: Expression,
}
