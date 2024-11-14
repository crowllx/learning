package main

import eval "../tree-walk"
import p "../tree-walk/parser"
import tok "../tree-walk/tokenizer"
import "core:testing"

@(test)
binary_expression_test :: proc(t: ^testing.T) {
    // testing equality binary expression
    expressions: map[string]tok.Literal = {
        "4 != 4" = false,
        "4 == 4" = true,
        "4 > 3"  = true,
        "4 < 3"  = false,
        "3 >= 3" = true,
        "3 <= 3" = true,
        "3 + 5"  = 8.0,
        "4/2"    = 2.0,
        "12 - 8" = 4.0,
        "11 * 4" = 44.0,
    }
    defer delete(expressions)

    tokenizer: tok.Tokenizer
    tokens: [dynamic]tok.Token
    defer tok.tokenizer_destroy(&tokenizer)
    defer delete(tokens)


    for k, expected in expressions {
        tok.tokenizer_reset(&tokenizer, k)
        clear(&tokens)
        tok.get_tokens(&tokenizer, &tokens)
        parser := p.parser_init(tokens[:])
        expr, _ := p.expression(&parser)
        defer p.expression_destory(expr)
        // defer p.expression_destory(expr)

        val, err := eval.evaluate_expr(expr)
        testing.expectf(
            t,
            val == expected,
            "result: %v expected: %v err: %v, expr: %v",
            val,
            expected,
            err,
            k,
        )
    }
}
