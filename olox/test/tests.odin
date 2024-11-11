package main

import ast "../tree-walk/ast"
import p "../tree-walk/parser"
import tok "../tree-walk/tokenizer"
import "core:fmt"
import "core:log"
import "core:strings"
import "core:testing"

@(test)
expression_test :: proc(t: ^testing.T) {

    t := tok.tokenizer_create("1 + 3")
    tokens: [dynamic]tok.Token
    defer tok.tokenizer_destroy(&t)
    defer delete(tokens)

    for {
        if token, ok := tok.tokenizer_next(&t); ok {
            append(&tokens, token)
        } else {
            break
        }
    }
    log.info(len(tokens))
    log.info(len(tokens))
    log.info(len(tokens))
    log.info(len(tokens))
    log.info(len(tokens))


    parser := p.parser_init(tokens[:])
    expr := p.parse(&parser)
    expr_str := ast.to_string(expr)
    defer delete(expr_str)
    // log.infof("%v", expr)
    // log.infof("%v", expr)
    // log.info(ast.to_string(&expr))
}


main :: proc() {
    t := tok.tokenizer_create("1 + ( 3 * 4)")
    tokens: [dynamic]tok.Token
    // defer tok.tokenizer_destroy(&t)
    defer delete(tokens)

    for {
        if token, ok := tok.tokenizer_next(&t); ok {
            append(&tokens, token)
        } else {
            break
        }
    }
    log.info(len(tokens))
    log.info(len(tokens))
    log.info(len(tokens))
    log.info(len(tokens))
    log.info(len(tokens))


    parser := p.parser_init(tokens[:])
    expr := p.parse(&parser)
    expr_str := ast.to_string(expr)
    defer delete(expr_str)
    fmt.printfln("%v", expr)
    fmt.printfln("%s", ast.to_string(expr))
}
