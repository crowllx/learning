package ast
import "core:fmt"
import "core:strings"

// Expression functions
// printer to help visualize the tree

// to_string :: proc(e: Expression) -> string {
//     str: string
//     switch v in e {
//     case ^Binary:
//         str = binary_to_string(v)
//     case ^Unary:
//         str = unary_to_string(v)
//     case ^Literal:
//         str = literal_to_string(v)
//     case ^Grouping:
//         str = group_to_string(v)
//     }
//     return str
// }

to_string :: proc {
    binary_to_string,
    group_to_string,
    unary_to_string,
    literal_to_string,
}

parenthesize :: proc(name: string, exprs: ..Expression) -> string {
    sb, err := strings.builder_make_none()
    defer strings.builder_destroy(&sb)

    fmt.println(len(exprs))
    fmt.println("parens")
    strings.write_string(&sb, "(")
    strings.write_string(&sb, name)

    for e in exprs {
        fmt.printfln("%v", e)
        strings.write_string(&sb, " ")
        switch v in e {
        case ^Binary:
            strings.write_string(&sb, to_string(v))
        case ^Unary:
            strings.write_string(&sb, to_string(v))
        case ^Grouping:
            strings.write_string(&sb, to_string(v))
        case ^Literal:
            strings.write_string(&sb, to_string(v))
        }
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

literal_to_string :: proc(e: ^Literal) -> string {
    fmt.println("literal reached")
    if e.value.type == .NIL do return "nil"
    return e.value.lexeme
}

group_to_string :: proc(e: ^Grouping) -> string {
    return parenthesize("group", e.expr)
}
