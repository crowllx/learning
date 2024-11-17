package parser
import ast "../ast"
import tok "../tokenizer"
import "core:fmt"
import "core:strings"


Parser :: struct {
    tokens:   []tok.Token,
    current:  int,
    prev_tok: tok.Token,
}

MissingToken :: struct {
    msg:   string,
    token: tok.Token,
}

UnexpectedToken :: struct {
    msg:   string,
    token: tok.Token,
}

ParsingError :: union {
    MissingToken,
    UnexpectedToken,
}

statement :: proc(p: ^Parser) -> (stmt: ast.Stmt, err: ParsingError) {
    if match(p, .VAR) {
        id := consume(p, .IDENTIFIER, "Expect varible name.") or_return
        decl: ast.Decl
        if match(p, .EQUAL) {
            decl.expr = expression(p) or_return
        }
        decl.id = strings.clone(id.lexeme)
        stmt = decl
    } else if match(p, .PRINT) {
        print_stmt: ast.Expr_Stmt
        print_stmt.type = .PRINT_STMT
        print_stmt.expr = expression(p) or_return
        stmt = print_stmt

    } else if match(p, .LEFT_BRACE) {
        block: ast.Block

        for (!check(p, .RIGHT_BRACE) && !finished(p)) {
            s: ast.Stmt
            s, err = statement(p)
            if err != nil {
                statement_destroy(s)
                for v in block.stmts {
                    statement_destroy(v)

                }
                delete(block.stmts)
                return nil, err
            }
            append(&block.stmts, s)
        }
        consume(p, .RIGHT_BRACE, "Expect '}' after block.") or_return

        stmt = block
    } else {
        expr_stmt: ast.Expr_Stmt

        expr_stmt.expr = expression(p) or_return
        expr_stmt.type = .EXPR_STMT
        stmt = expr_stmt
    }

    consume(p, .SEMICOLON, "Expect ';' after value.") or_return
    return stmt, nil
}

parser_init :: proc(ts: []tok.Token) -> Parser {
    return Parser{tokens = ts}
}

statement_destroy :: proc(stmt: ast.Stmt) {
    switch v in stmt {
    case ast.Expr_Stmt:
        expression_destroy(v.expr)
    case ast.Decl:
        delete(v.id)
        expression_destroy(v.expr)
    case ast.Block:
        for s in v.stmts {
            statement_destroy(s)
        }
        delete(v.stmts)
    }
}

// memory cleanup for expression trees
expression_destroy :: proc(e: ast.Expr) {
    switch v in e {
    case ^ast.Binary:
        expression_destroy(v.left_expr)
        expression_destroy(v.right_expr)
        free(v)
    case ^ast.Grouping:
        expression_destroy(v.expr)
        free(v)
    case ^ast.Unary:
        expression_destroy(v.expr)
        free(v)
    case ^ast.LiteralExpr:
        delete(v.lexeme)
        free(v)
    case ^ast.Variable:
        delete(v.lexeme)
        free(v)
    case ^ast.Assignment:
        expression_destroy(v.value)
        free(v)
    }
}


// parse a sequence of tokens into statements
parse :: proc(source: string, allocator := context.allocator) -> ([]ast.Stmt, []ParsingError) {
    tokenizer := tok.tokenizer_create(source)
    tokens: [dynamic]tok.Token
    tok.get_tokens(&tokenizer, &tokens)
    buf: [dynamic]ast.Stmt
    errors: [dynamic]ParsingError
    encountered_err := false
    parser := parser_init(tokens[:])

    defer {
        tok.tokenizer_destroy(&tokenizer)
        delete(tokens)
    }

    for !finished(&parser) {
        stmt, err := statement(&parser)
        if err != nil {
            statement_destroy(stmt)
            encountered_err = true
            append(&errors, err)
        } else {
            append(&buf, stmt)
        }
    }
    return buf[:], errors[:]
}


expression :: proc(p: ^Parser) -> (expr: ast.Expr, err: ParsingError) {
    return assignment(p)
}

@(private)
assignment :: proc(p: ^Parser) -> (expr: ast.Expr, err: ParsingError) {
    expr = equality(p) or_return

    if match(p, .EQUAL) {
        v, ok := expr.(^ast.Variable)
        if !ok {
            return nil, UnexpectedToken{token = p.prev_tok, msg = "Expected identifier"}
        }
        value := expression(p) or_return
        expr = new_clone(ast.Assignment{identifier = v, value = value})
    }

    return expr, err

}

@(private)
equality :: proc(p: ^Parser) -> (expr: ast.Expr, err: ParsingError) {
    expr = comparison(p) or_return

    for match(p, .BANG_EQUAL, .EQUAL_EQUAL) {
        op := p.prev_tok
        right := comparison(p) or_return
        expr = new_clone(ast.Binary{left_expr = expr, operator = op, right_expr = right})
    }

    return expr, nil
}

@(private)
comparison :: proc(p: ^Parser) -> (expr: ast.Expr, err: ParsingError) {
    expr = term(p) or_return

    for match(p, .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL) {
        op := p.prev_tok
        right := term(p) or_return
        expr = new_clone(ast.Binary{left_expr = expr, operator = op, right_expr = right})

    }
    return expr, nil

}

@(private)
term :: proc(p: ^Parser) -> (expr: ast.Expr, err: ParsingError) {
    expr = factor(p) or_return

    for match(p, .MINUS, .PLUS) {
        op := p.prev_tok
        right := factor(p) or_return
        expr = new_clone(ast.Binary{left_expr = expr, operator = op, right_expr = right})
    }
    return expr, nil
}

@(private)
factor :: proc(p: ^Parser) -> (expr: ast.Expr, err: ParsingError) {
    expr = unary(p) or_return

    for match(p, .STAR, .SLASH) {
        op := p.prev_tok
        right := unary(p) or_return
        expr = new_clone(ast.Binary{left_expr = expr, operator = op, right_expr = right})
    }
    return expr, nil
}

@(private)
unary :: proc(p: ^Parser) -> (expr: ast.Expr, err: ParsingError) {
    if match(p, .BANG, .MINUS) {
        op := p.prev_tok
        right := unary(p) or_return
        return new_clone(ast.Unary{operator = op, expr = right}), nil
    }

    return primary(p)
}


@(private)
primary :: proc(p: ^Parser) -> (exor: ast.Expr, err: ParsingError) {
    if match(p, .NUMBER, .STRING, .FALSE, .TRUE, .NIL) {
        return new_clone(
                ast.LiteralExpr {
                    literal = p.prev_tok.literal,
                    lexeme = strings.clone(p.prev_tok.lexeme),
                },
            ),
            nil
    }

    if match(p, .IDENTIFIER) {
        return new_clone(
                tok.Token {
                    lexeme = strings.clone(p.prev_tok.lexeme),
                    literal = p.prev_tok.literal,
                    type = p.prev_tok.type,
                    line = p.prev_tok.line,
                },
            ),
            nil
    }


    if match(p, .LEFT_PAREN) {
        expr := expression(p) or_return
        consume(p, .RIGHT_PAREN, "Expect ')' after expression.")
        return new_clone(ast.Grouping{expr = expr}), nil
    }

    advance(p)
    return nil, UnexpectedToken{msg = "Unexpected token: ", token = peek(p)}
}


@(private)
synchronize :: proc(p: ^Parser) {
    advance(p)
    boundaries :: bit_set[tok.TokenType]{.CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN}

    for !finished(p) {
        if p.prev_tok.type == .SEMICOLON do break
        if peek(p).type in boundaries do break
        advance(p)
    }
}


// helpers

@(private = "file")
consume :: proc(p: ^Parser, type: tok.TokenType, msg: string) -> (tok.Token, ParsingError) {
    if check(p, type) do return advance(p), nil
    // some error handling need to happen
    next_token := peek(p)
    return next_token, MissingToken{msg = msg, token = next_token}
}

@(private)
error :: proc(p: ^Parser, token: tok.Token, msg: string) {
    if token.type == .EOF {
        fmt.eprintfln("%d at end of file %s", token.line, msg)
    } else {
        fmt.eprintfln("%d at '%s' %s", token.line, token.lexeme, msg)
    }
}

@(private)
finished :: proc(p: ^Parser) -> bool {
    return p.current >= len(p.tokens)
}

@(private)
match :: proc(p: ^Parser, types: ..tok.TokenType) -> bool {
    for t in types {
        if check(p, t) {
            advance(p)
            return true
        }
    }
    return false
}

@(private)
check :: proc(p: ^Parser, token_type: tok.TokenType) -> bool {
    if finished(p) do return false
    return peek(p).type == token_type
}

@(private)
advance :: proc(p: ^Parser) -> tok.Token {
    t := peek(p)
    if !finished(p) {
        p.current += 1
        p.prev_tok = t
    }
    return t
}

@(private)
peek :: proc(p: ^Parser) -> tok.Token {
    if p.current < len(p.tokens) {
        return p.tokens[p.current]
    }
    return p.prev_tok
}
