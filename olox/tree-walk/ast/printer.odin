package ast
import "core:strings"

// Expression functions
// printer to help visualize the tree

expr_to_string :: proc(e: Expression) -> string {
    str: string
    switch &v in e {
    case ^Binary:
        str = binary_to_string(v)
    case ^Unary:
        str = unary_to_string(v)
    case ^LiteralExpr:
        str = literal_to_string(v)
    case ^Grouping:
        str = group_to_string(v)
    }
    return str
}

to_string :: proc {
    binary_to_string,
    group_to_string,
    unary_to_string,
    literal_to_string,
    expr_to_string,
}

parenthesize :: proc(name: string, exprs: ..Expression) -> string {
    sb, _ := strings.builder_make_none()
    defer strings.builder_destroy(&sb)

    strings.write_string(&sb, "(")
    strings.write_string(&sb, name)

    for e in exprs {
        strings.write_string(&sb, " ")
        sub_expr := to_string(e)
        defer delete(sub_expr)

        strings.write_string(&sb, sub_expr)
    }
    strings.write_string(&sb, ")")
    return strings.clone(strings.to_string(sb))
}

binary_to_string :: proc(e: ^Binary) -> string {
    return parenthesize(e.operator.lexeme, e.left_expr, e.right_expr)
}

unary_to_string :: proc(e: ^Unary) -> string {
    return parenthesize(e.operator.lexeme, e.expr)
}

literal_to_string :: proc(e: ^LiteralExpr) -> string {
    return strings.clone(e.lexeme)
}

group_to_string :: proc(e: ^Grouping) -> string {
    return parenthesize("group", e.expr)
}
