package main

import ast "../tree-walk/ast"
import tok "../tree-walk/tokenizer"
import "core:fmt"
import "core:strings"

@(test)
main :: proc() {
    somestr := "func传伡伢伣"

    fmt.printfln("string length: %d", len(somestr))


    fmt.printfln("rune comp: %v", '1')
    e: ast.Binary
    e.left_expr =
    &ast.Unary {
        operator = tok.Token{type = .MINUS, lexeme = "-"},
        expr = &ast.Literal{value = tok.Token{type = .NUMBER, lexeme = "123"}},
    }

    e.operator = tok.Token {
        type   = .STAR,
        lexeme = "*",
    }
    e.right_expr =
    &ast.Grouping {
        expr = &ast.Literal {
            value = tok.Token{type = .NUMBER, lexeme = "45.67"},
        },
    }

    tree := ast.to_string(&e)
    fmt.println(tree)
}
