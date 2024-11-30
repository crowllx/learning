package parser
import ast "../ast"
import tok "../tokenizer"
import "core:fmt"
import "core:log"
import "core:strings"


Parser :: struct {
    tokens:   []tok.Token,
    current:  int,
    prev_tok: tok.Token,
}

Missing_Token :: struct {
    msg:   string,
    token: tok.Token,
}

Unexpected_Token :: struct {
    msg:   string,
    token: tok.Token,
}

Parsing_Error :: union {
    Missing_Token,
    Unexpected_Token,
}

declaration :: proc(p: ^Parser) -> (stmt: ast.Stmt, err: Parsing_Error) {
    id := consume(p, .IDENTIFIER, "Expect variable name.") or_return
    decl: ast.Decl
    if match(p, .EQUAL) {
        decl.expr = expression(p) or_return
    }
    decl.id = strings.clone(id.lexeme)
    stmt = decl
    return stmt, nil
}

for_loop :: proc(p: ^Parser) -> (stmt: ast.Stmt, err: Parsing_Error) {
    consume(p, .LEFT_PAREN, "Expect ( after 'for'.")

    initializer: ast.Stmt
    condition: ast.Expr
    increment: ast.Expr
    body: ast.Stmt

    if match(p, .SEMICOLON) {
    } else if match(p, .VAR) {
        initializer = declaration(p) or_return
    } else {
        expr_stmt: ast.Expr_Stmt
        expr_stmt.expr = expression(p) or_return
        expr_stmt.type = .EXPR_STMT
        initializer = expr_stmt
    }
    consume(p, .SEMICOLON, "Expect ';' after for loop initializer") or_return

    if (!check(p, .SEMICOLON)) {
        condition = expression(p) or_return
    }
    consume(p, .SEMICOLON, "Expect ';' after loop condition") or_return


    if (!check(p, .RIGHT_PAREN)) {
        increment = expression(p) or_return
    }

    consume(p, .RIGHT_PAREN, "Expect ')' after for clauses.") or_return
    body = statement(p) or_return

    // de sugar to while loop
    if increment != nil {
        expr: ast.Expr_Stmt
        expr.expr = increment
        expr.type = .EXPR_STMT
        body = ast.Block {
            stmts = {body, expr},
        }
    }
    if condition == nil {
        condition = new_clone(ast.Literal_Expr{literal = true})
    }

    body = ast.While_Stmt {
        condition = condition,
        body      = new_clone(body),
    }

    if initializer != nil {
        body = ast.Block {
            stmts = {initializer, body},
        }
    }
    return body, nil
}

function :: proc(p: ^Parser, kind: string) -> (stmt: ast.Function, err: Parsing_Error) {
    name := consume(p, .IDENTIFIER, "Expect function name.") or_return
    consume(p, .LEFT_PAREN, "Expect '(' after function name.") or_return
    params: [dynamic]string
    if !check(p, .RIGHT_PAREN) {
        for {
            if len(params) >= 255 {
                error(p, peek(p), "Can't have more than 255 parameters")
                // return error here instead
            }
            token := consume(p, .IDENTIFIER, "Expect parameter name.") or_return
            append(&params, token.lexeme)
            if !match(p, .COMMA) do break
        }
    }
    consume(p, .RIGHT_PAREN, "Expect ')' after parameters.")

    consume(p, .LEFT_BRACE, "Expect '{', before function body.") or_return
    body := block(p) or_return
    return ast.Function {
            params = params[:],
            name = strings.clone(name.lexeme),
            body = body.(ast.Block),
        },
        nil
}

block :: proc(p: ^Parser) -> (stmt: ast.Stmt, err: Parsing_Error) {

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
    return block, nil
}
statement :: proc(p: ^Parser) -> (stmt: ast.Stmt, err: Parsing_Error) {
    if match(p, .VAR) {
        stmt = declaration(p) or_return
    } else if match(p, .FUN) {
        return function(p, "function")
    } else if match(p, .FOR) {
        return for_loop(p)
    } else if match(p, .IF) {
        consume(p, .LEFT_PAREN, "Expect '(' after 'if'.")
        condition := expression(p) or_return
        consume(p, .RIGHT_PAREN, "Expect ')' after if condition.")

        then := statement(p) or_return
        else_branch: ast.Stmt
        if match(p, .ELSE) {
            else_branch = statement(p) or_return
        }

        if_stmt := ast.If_Stmt {
            condition = condition,
            then_stmt = new_clone(then),
            else_stmt = new_clone(else_branch),
        }

        return if_stmt, nil
    } else if match(p, .PRINT) {
        print_stmt: ast.Expr_Stmt
        print_stmt.type = .PRINT_STMT
        print_stmt.expr = expression(p) or_return
        stmt = print_stmt

    } else if match(p, .RETURN) {
        ret_stmt: ast.Return_Stmt
        expr: ast.Expr = nil

        if !check(p, .SEMICOLON) {
            expr = expression(p) or_return
        }
        ret_stmt.value = expr
        stmt = ret_stmt

    } else if match(p, .WHILE) {
        consume(p, .LEFT_PAREN, "Expect ( after 'while'.") or_return
        condition := expression(p) or_return
        consume(p, .RIGHT_PAREN, "Expect ) after condition.") or_return

        body := statement(p) or_return

        return ast.While_Stmt{condition = condition, body = new_clone(body)}, nil

    } else if match(p, .LEFT_BRACE) {
        return block(p)
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
    case ast.If_Stmt:
        expression_destroy(v.condition)
        statement_destroy(v.then_stmt^)
        statement_destroy(v.else_stmt^)
        free(v.then_stmt)
        free(v.else_stmt)
    case ast.While_Stmt:
        expression_destroy(v.condition)
        statement_destroy(v.body^)
        free(v.body)
    case ast.Block:
        for s in v.stmts {
            statement_destroy(s)
        }
        delete(v.stmts)
    case ast.Function:
        statement_destroy(v.body)
        delete(v.name)
        delete(v.params)
    case ast.Return_Stmt:
        expression_destroy(v.value)
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
    case ^ast.Literal_Expr:
        if lit_string, ok := v.literal.(string); ok {
            delete(lit_string)
        }
        delete(v.lexeme)
        free(v)
    case ^ast.Variable:
        delete(v.lexeme)
        free(v)
    case ^ast.Assignment:
        expression_destroy(v.value)
        expression_destroy(v.identifier)
        free(v)
    case ^ast.Logic_Expr:
        expression_destroy(v.right_expr)
        expression_destroy(v.left_expr)
        free(v)

    case ^ast.Call:
        expression_destroy(v.callee)
        for expr in v.args {
            expression_destroy(expr)
        }
        delete(v.args)
        free(v)
    }
}


// parse a sequence of tokens into statements
parse :: proc(source: string, allocator := context.allocator) -> ([]ast.Stmt, []Parsing_Error) {
    tokenizer := tok.tokenizer_create(source)
    tokens: [dynamic]tok.Token
    tok.get_tokens(&tokenizer, &tokens)
    buf: [dynamic]ast.Stmt
    errors: [dynamic]Parsing_Error
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


expression :: proc(p: ^Parser) -> (expr: ast.Expr, err: Parsing_Error) {
    expr, err = assignment(p)
    if err != nil {
        fmt.printfln("freeing")
        free_all(context.temp_allocator)
    } else {
        expr = ast.copy_expr(expr)
    }
    return
}

@(private)
assignment :: proc(
    p: ^Parser,
    allocator := context.temp_allocator,
) -> (
    expr: ast.Expr,
    err: Parsing_Error,
) {
    context.allocator = allocator
    expr = logical(p) or_return

    if match(p, .EQUAL) {
        v, ok := expr.(^ast.Variable)
        if !ok {
            return nil, Unexpected_Token{token = p.prev_tok, msg = "Expected identifier"}
        }
        value := expression(p) or_return
        expr = new_clone(ast.Assignment{identifier = v, value = value})
    }

    return expr, err

}

logical :: proc(p: ^Parser) -> (expr: ast.Expr, err: Parsing_Error) {
    expr = equality(p) or_return

    for match(p, .AND, .OR) {
        logical_expr := new(ast.Logic_Expr)
        logical_expr.operator = p.prev_tok
        logical_expr.right_expr = equality(p) or_return
        logical_expr.left_expr = expr
        expr = logical_expr
    }

    return expr, err
}

@(private)
equality :: proc(p: ^Parser) -> (expr: ast.Expr, err: Parsing_Error) {
    expr = comparison(p) or_return

    for match(p, .BANG_EQUAL, .EQUAL_EQUAL) {
        op := p.prev_tok
        right := comparison(p) or_return
        expr = new_clone(ast.Binary{left_expr = expr, operator = op, right_expr = right})
    }

    return expr, nil
}

@(private)
comparison :: proc(p: ^Parser) -> (expr: ast.Expr, err: Parsing_Error) {
    expr = term(p) or_return

    for match(p, .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL) {
        op := p.prev_tok
        right := term(p) or_return
        expr = new_clone(ast.Binary{left_expr = expr, operator = op, right_expr = right})

    }
    return expr, nil

}

@(private)
term :: proc(p: ^Parser) -> (expr: ast.Expr, err: Parsing_Error) {
    expr = factor(p) or_return

    for match(p, .MINUS, .PLUS) {
        op := p.prev_tok
        right := factor(p) or_return
        expr = new_clone(ast.Binary{left_expr = expr, operator = op, right_expr = right})
    }
    return expr, nil
}

@(private)
factor :: proc(p: ^Parser) -> (expr: ast.Expr, err: Parsing_Error) {
    expr = unary(p) or_return

    for match(p, .STAR, .SLASH) {
        op := p.prev_tok
        right := unary(p) or_return
        expr = new_clone(ast.Binary{left_expr = expr, operator = op, right_expr = right})
    }
    return expr, nil
}

@(private)
unary :: proc(p: ^Parser) -> (expr: ast.Expr, err: Parsing_Error) {
    if match(p, .BANG, .MINUS) {
        op := p.prev_tok
        right := unary(p) or_return
        return new_clone(ast.Unary{operator = op, expr = right}), nil
    }

    return function_call(p)
}


@(private)
build_function_call :: proc(p: ^Parser, callee: ast.Expr) -> (expr: ast.Expr, err: Parsing_Error) {
    args: [dynamic]ast.Expr
    if !check(p, .RIGHT_PAREN) {
        for {
            arg := expression(p) or_return
            append(&args, arg)

            if len(args) > 256 {
                return nil, Unexpected_Token{msg = "Can't have more than 255 arguments."}
            }
            if !match(p, .COMMA) do break
        }
    }

    paren := consume(p, .RIGHT_PAREN, "Expect ')' after arguments.") or_return

    return new_clone(ast.Call{args = args[:], paren = paren, callee = callee}), nil
}

@(private)
function_call :: proc(p: ^Parser) -> (expr: ast.Expr, err: Parsing_Error) {
    expr = primary(p) or_return
    for {
        if match(p, .LEFT_PAREN) {
            expr = build_function_call(p, expr) or_return
        } else {
            break
        }
    }
    return expr, nil
}

@(private)
primary :: proc(p: ^Parser) -> (exor: ast.Expr, err: Parsing_Error) {
    if match(p, .NUMBER, .STRING, .FALSE, .TRUE, .NIL) {
        lit := p.prev_tok.literal
        lex := strings.clone(p.prev_tok.lexeme)
        if p.prev_tok.type == .STRING {
            lit = strings.clone(p.prev_tok.literal.(string))
        }
        return new_clone(ast.Literal_Expr{literal = lit, lexeme = lex}), nil
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
    return nil, Unexpected_Token{msg = "Unexpected token: ", token = peek(p)}
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

// if this function can be made to either delete or clone msg based on
// result, and have additional cleanup for errors, we could use more dynamic error msgs
@(private = "file")
consume :: proc(p: ^Parser, type: tok.TokenType, msg: string) -> (tok.Token, Parsing_Error) {
    if check(p, type) do return advance(p), nil
    // some error handling need to happen
    next_token := peek(p)
    return next_token, Missing_Token{msg = msg, token = next_token}
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
