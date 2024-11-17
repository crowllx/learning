package tree

import "ast"
import "core:fmt"
import "core:mem"
import "core:strings"
import "parser"
import tok "tokenizer"

@(private = "file")
env := [dynamic]map[string]tok.Literal{make(map[string]tok.Literal)}


EvaluationError :: struct {
    msg:  string,
    line: int,
}

TypeError :: struct {
    left:  tok.Literal,
    right: tok.Literal,
    msg:   string,
    line:  int,
}

UndefinedVar :: struct {
    name: string,
    line: int,
}

InterpretorError :: union {
    EvaluationError,
    TypeError,
    mem.Allocator_Error,
    parser.UnexpectedToken,
    parser.MissingToken,
    UndefinedVar,
}

interpret :: proc(stmts: []ast.Stmt, errs: []parser.ParsingError) {
    for s in stmts {
        execute_stmt(s) or_break
    }

    for e in errs {
        switch err in e {
        case parser.UnexpectedToken:
            report_error(err)
        case parser.MissingToken:
            report_error(err)
        }
    }
}

execute_stmt :: proc(stmt: ast.Stmt) -> InterpretorError {
    val: tok.Literal
    err: InterpretorError

    switch v in stmt {
    // any expression or print
    case ast.Expr_Stmt:
        val, err = eval(v.expr)
        switch v.type {
        case .PRINT_STMT:
            fmt.println(val)
        case .EXPR_STMT:
        }

    // declare variable in innermost scope
    case ast.Decl:
        val, err = eval(v.expr)
        scope := &env[len(env) - 1]
        if v.id in scope {
            scope[v.id] = val
        } else {
            scope[strings.clone(v.id)] = val
        }
        return nil

    // create new scope before running all of the statements within this block
    case ast.Block:
        append(&env, map[string]tok.Literal{})
        for statement in v.stmts {
            execute_stmt(statement)
        }
        cleanup_variables()
    }


    if s, ok := val.(string); ok do delete(s)
    return err
}

report_error :: proc(err: InterpretorError) {
    switch e in err {
    case EvaluationError:
        fmt.printfln("%s, [line %d]", e.msg, e.line)
    case TypeError:
        fmt.printfln("%s, [line %d]", e.msg, e.line)
    case parser.MissingToken:
        fmt.printfln("%s [line %d]", e.msg, e.token.line)
    case parser.UnexpectedToken:
        fmt.printfln("%s %v [ line %d]", e.msg, e.token.lexeme, e.token.line)
    case mem.Allocator_Error:
        fmt.eprintln(e)
    case UndefinedVar:
        fmt.eprintfln("Undefined Variable %s [line %d]", e.name, e.line)
    case:
    }
}

// wrapper for recursive function evaluate_expr, allows clean up
// temporary allocator once the expression is evaluated
eval :: proc(
    expr: ast.Expr,
    allocator := context.allocator,
) -> (
    val: tok.Literal,
    err: InterpretorError,
) {
    val, err = evaluate_expr(expr, context.temp_allocator)
    if err != nil {
        report_error(err)
        return nil, err
    }
    if s, ok := val.(string); ok do val = strings.clone(s, allocator)
    free_all(context.temp_allocator)
    return val, err
}

@(private = "file")
evaluate_expr :: proc(
    expr: ast.Expr,
    allocator := context.temp_allocator,
) -> (
    val: tok.Literal,
    err: InterpretorError,
) {
    switch t in expr {
    case ^ast.Unary:
        val = evaluate_unary(t) or_return
    case ^ast.Binary:
        val = eval_binary(t) or_return
        if str, ok := val.(string); ok do val = strings.clone(str, allocator)
    case ^ast.Grouping:
        val, err = evaluate_expr(t.expr)
    case ^ast.LiteralExpr:
        val = t.literal
    case ast.Variable:
        val = get_variable(t.lexeme) or_return
    case ^ast.Assignment:
        val = evaluate_expr(t.value) or_return
        scope := &env[len(env) - 1]
        if !(t.identifier.lexeme in scope) {
            return val, UndefinedVar{name = t.identifier.lexeme, line = t.identifier.line}
        }
        scope[t.identifier.lexeme] = val
    }

    return val, nil
}

evaluate_unary :: proc(expr: ^ast.Unary) -> (val: tok.Literal, err: InterpretorError) {
    right := evaluate_expr(expr.expr) or_return

    #partial switch expr.operator.type {
    case .MINUS:
        val = -right.(f64)
    case .BANG:
        val = right == nil || right == false
    case:
        return nil, EvaluationError{msg = "Unexpected Unary Operator", line = expr.operator.line}
    }

    return val, nil
}

check_number :: proc(left: tok.Literal, right: tok.Literal, line: int) -> InterpretorError {
    _, left_ok := left.(f64)
    _, right_ok := right.(f64)
    if !left_ok || !right_ok {
        return TypeError {
            left = left,
            right = right,
            msg = "Invalid types for arithmetic expression",
            line = line,
        }
    }
    return nil
}

check_string :: proc(left: tok.Literal, right: tok.Literal, line: int) -> InterpretorError {
    _, left_ok := left.(string)
    _, right_ok := right.(string)
    if !left_ok || !right_ok {
        return TypeError {
            left = left,
            right = right,
            msg = "Invalid types for string concatenation",
            line = line,
        }
    }
    return nil
}

eval_binary :: proc(expr: ^ast.Binary) -> (val: tok.Literal, err: InterpretorError) {
    left := evaluate_expr(expr.left_expr) or_return
    right := evaluate_expr(expr.right_expr) or_return

    #partial switch expr.operator.type {
    case .MINUS:
        check_number(left, right, expr.operator.line) or_return
        val = left.(f64) - right.(f64)
    case .SLASH:
        check_number(left, right, expr.operator.line) or_return
        val = left.(f64) / right.(f64)
    case .STAR:
        check_number(left, right, expr.operator.line) or_return
        val = left.(f64) * right.(f64)
    case .PLUS:
        #partial switch t in left {
        case f64:
            check_number(left, right, expr.operator.line) or_return
            val = left.(f64) + right.(f64)
        case string:
            check_string(left, right, expr.operator.line) or_return
            val = strings.concatenate(
                {left.(string), right.(string)},
                context.temp_allocator,
            ) or_return
        }
    case .GREATER:
        val = left.(f64) > right.(f64)
    case .GREATER_EQUAL:
        val = left.(f64) >= right.(f64)
    case .LESS:
        val = left.(f64) < right.(f64)
    case .LESS_EQUAL:
        val = left.(f64) <= right.(f64)
    case .BANG_EQUAL:
        val = left != right
    case .EQUAL_EQUAL:
        val = left == right
    case:
        return nil, EvaluationError {
            msg = "Unexpected operator in binary expression",
            line = expr.operator.line,
        }
    }
    return val, nil
}

get_variable :: proc(variable_name: string) -> (tok.Literal, InterpretorError) {
    #reverse for scope in env {
        if v, ok := scope[variable_name]; ok {
            return v, nil
        }
    }

    return nil, UndefinedVar{name = variable_name}
}

// clean up allocations for global environment
cleanup :: proc() {
    length := len(env)
    for i := 0; i < length; i += 1 {
        cleanup_variables()
    }
}

@(private = "file")
cleanup_variables :: proc() {
    m := pop(&env)
    fmt.printfln("cleaning %v", m)
    for k, v in m {
        delete(k)
        if s, ok := v.(string); ok do delete(s)
    }
    delete(m)
}
