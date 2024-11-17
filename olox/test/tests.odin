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
        defer p.expression_destroy(expr)

        val, err := eval.eval(expr)
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

@(test)
single_statements_test :: proc(t: ^testing.T) {
    tests := map[string]bool {
        "var x = 13;"     = false,
        "var x = 13"      = true,
        "{ var y = 3 }"   = true,
        "{ var x = 3; }"  = false,
        "1 < 3 == false;" = false,
        "1 < 3 == false"  = true,
    }
    defer delete(tests)

    for key, should_err in tests {
        stmts, errs := p.parse(key)
        defer {
            for s in stmts {
                p.statement_destroy(s)
            }
            delete(stmts)
            delete(errs)
        }
        ls := len(stmts)
        le := len(errs)
        if should_err {
            testing.expectf(t, le > 0, "expected atleast 1 error")
        } else {
            testing.expectf(t, ls > 0, "expected atleast 1 statement input: %s %v", key, errs)
        }
    }
}
