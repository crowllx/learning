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
        env := eval.env_init()
        clear(&tokens)
        tok.get_tokens(&tokenizer, &tokens)
        parser := p.parser_init(tokens[:])
        expr, _ := p.expression(&parser)
        defer p.expression_destroy(expr)
        defer eval.cleanup(&env)

        val, err := eval.eval(&env, expr)
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


@(test)
if_statement_test :: proc(t: ^testing.T) {
    stmt_true := "if (true) 5; else 7;"
    stmt_false := "if (false) 5; else 7;"
    env := eval.env_init()
    defer eval.cleanup(&env)

    s1, _ := p.parse(stmt_true)
    defer p.statement_destroy(s1[0])
    defer delete(s1)

    v1, _ := eval.execute_stmt(&env, s1[0])
    testing.expectf(t, v1 == 5, "v: %v expected: 5", v1)

    s2, _ := p.parse(stmt_false)
    defer p.statement_destroy(s2[0])
    defer delete(s2)


    v2, _ := eval.execute_stmt(&env, s2[0])
    testing.expectf(t, v2 == 7, "v: %v expected: 7", v2)
}


@(test)
while_statement_test :: proc(t: ^testing.T) {
    stmt := "var i = 0; while (i < 5) { i = i + 1; }"
    s1, _ := p.parse(stmt)
    env := eval.env_init()
    defer {
        for s in s1 {
            p.statement_destroy(s)
        }
        delete(s1)
        eval.cleanup(&env)
    }

    eval.execute_stmt(&env, s1[0])
    v, _ := eval.execute_stmt(&env, s1[1])
    testing.expectf(t, v == 5, "v: %v expected: 5, err: %v", v, s1[1])
}

@(test)
for_loop_test :: proc(t: ^testing.T) {
    stmt := "var x = 5; for (var i = 0; i < 5; i = i + 1) { x = x - 1;} x;"
    parsed_stmts, _ := p.parse(stmt)
    env := eval.env_init()
    defer {
        for s in parsed_stmts {
            p.statement_destroy(s)
        }
        delete(parsed_stmts)
        eval.cleanup(&env)
    }
    eval.execute_stmt(&env, parsed_stmts[0])
    eval.execute_stmt(&env, parsed_stmts[1])
    v, _ := eval.execute_stmt(&env, parsed_stmts[2])

    testing.expectf(t, v == 0, "v: %v expected: 0, err: %v", v, parsed_stmts[1])
}
