package parser
import ast "../ast"
import tok "../tokenizer"
import "core:fmt"


Parser :: struct {
    tokens:   []tok.Token,
    current:  int,
    prev_tok: tok.Token,
}

ParsingError :: struct {
    msg: string,
}

Error :: union {
    ParsingError,
}

parser_init :: proc(ts: []tok.Token) -> Parser {
    return Parser{tokens = ts}
}

// memory cleanup for expression trees
expression_destory :: proc(e: ast.Expression) {
    switch v in e {
    case ^ast.Binary:
        expression_destory(v.left_expr)
        expression_destory(v.right_expr)
        free(v)
    case ^ast.Grouping:
        expression_destory(v.expr)
        free(v)
    case ^ast.Unary:
        expression_destory(v.expr)
        free(v)
    case ^ast.LiteralExpr:
        free(v)
    }
}

// generate an expression tree from a list of tokens
parse :: proc(p: ^Parser) -> ast.Expression {
    return expression(p)
}


// private
@(private)
expression :: proc(p: ^Parser) -> ast.Expression {
    return equality(p)
}

@(private)
equality :: proc(p: ^Parser) -> ast.Expression {
    expr := comparison(p)

    for match(p, .BANG_EQUAL, .EQUAL_EQUAL) {
        op := p.prev_tok
        right := comparison(p)
        expr = new_clone(
            ast.Binary{left_expr = expr, operator = op, right_expr = right},
        )
    }

    return expr
}

@(private)
comparison :: proc(p: ^Parser) -> ast.Expression {
    expr := term(p)

    for match(p, .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL) {
        op := p.prev_tok
        right := term(p)
        expr = new_clone(
            ast.Binary{left_expr = expr, operator = op, right_expr = right},
        )

    }
    return expr

}

@(private)
term :: proc(p: ^Parser) -> ast.Expression {
    expr := factor(p)

    for match(p, .MINUS, .PLUS) {
        op := p.prev_tok
        right := factor(p)
        expr = new_clone(
            ast.Binary{left_expr = expr, operator = op, right_expr = right},
        )
    }
    return expr
}

@(private)
factor :: proc(p: ^Parser) -> ast.Expression {
    expr := unary(p)

    for match(p, .STAR, .SLASH) {
        op := p.prev_tok
        right := unary(p)
        expr = new_clone(
            ast.Binary{left_expr = expr, operator = op, right_expr = right},
        )
    }
    return expr
}

@(private)
unary :: proc(p: ^Parser) -> ast.Expression {
    if match(p, .BANG, .MINUS) {
        op := p.prev_tok
        right := unary(p)
        return new_clone(ast.Unary{operator = op, expr = right})
    }

    return primary(p)
}


@(private)
primary :: proc(p: ^Parser) -> ast.Expression {
    if match(p, .NUMBER, .STRING, .FALSE, .TRUE, .NIL) {
        return new_clone(
            ast.LiteralExpr {
                literal = p.prev_tok.literal,
                lexeme = p.prev_tok.lexeme,
            },
        )
    }

    if match(p, .LEFT_PAREN) {
        expr := expression(p)
        consume(p, .RIGHT_PAREN, "Expect ')' after expression.")
        return new_clone(ast.Grouping{expr = expr})
    }
    error(p, peek(p), "Unkown expr")
    return nil
}


@(private)
synchronize :: proc(p: ^Parser) {
    advance(p)
    boundaries :: bit_set[tok.TokenType] {
        .CLASS,
        .FUN,
        .VAR,
        .FOR,
        .IF,
        .WHILE,
        .PRINT,
        .RETURN,
    }

    for !finished(p) {
        if p.prev_tok.type == .SEMICOLON do break
        if peek(p).type in boundaries do break
        advance(p)
    }
}


// helpers

@(private)
consume :: proc(p: ^Parser, type: tok.TokenType, msg: string) -> tok.Token {
    if check(p, type) do return advance(p)
    // some error handling need to happen
    return peek(p)
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
