package tree

import "ast"
import "core:fmt"
import "core:mem"
import "core:strings"
import "parser"
import tok "tokenizer"


Env :: struct {
    scopes: [dynamic]map[string]tok.Literal,
}


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
    parser.Unexpected_Token,
    parser.Missing_Token,
    UndefinedVar,
}

interpret :: proc(env: ^Env, stmts: []ast.Stmt, errs: []parser.Parsing_Error) {
    for s in stmts {
        execute_stmt(env, s) or_break
    }

    for e in errs {
        switch err in e {
        case parser.Unexpected_Token:
            report_error(err)
        case parser.Missing_Token:
            report_error(err)
        }
    }
}

execute_stmt :: proc(env: ^Env, stmt: ast.Stmt) -> (val: tok.Literal, err: InterpretorError) {

    switch v in stmt {
    // any expression or print
    case ast.Expr_Stmt:
        val = eval(env, v.expr) or_return
        switch v.type {
        case .PRINT_STMT:
            fmt.println(val)
        case .EXPR_STMT:
        }

    // declare variable in innermost scope
    case ast.Decl:
        val, err = eval(env, v.expr)
        scope := &env.scopes[len(env.scopes) - 1]
        if v.id in scope {
            scope[v.id] = val
        } else {
            scope[strings.clone(v.id)] = val
        }
        return val, nil

    // create new scope before running all of the statements within this block
    case ast.If_Stmt:
        c := eval(env, v.condition) or_return
        if c == false || c == nil {
            val = execute_stmt(env, v.else_stmt^) or_return
        } else {
            val = execute_stmt(env, v.then_stmt^) or_return
        }

    case ast.While_Stmt:
        condition := eval(env, v.condition) or_return
        for is_truthy(condition) {
            val = execute_stmt(env, v.body^) or_return
            condition = eval(env, v.condition) or_return
        }

    case ast.Block:
        append(&env.scopes, map[string]tok.Literal{})
        for statement in v.stmts {
            val = execute_stmt(env, statement) or_return
        }
        cleanup_variables(env)
    }


    if s, ok := val.(string); ok do delete(s)
    return val, err
}

report_error :: proc(err: InterpretorError) {
    switch e in err {
    case EvaluationError:
        fmt.printfln("%s, [line %d]", e.msg, e.line)
    case TypeError:
        fmt.printfln("%s, [line %d]", e.msg, e.line)
    case parser.Missing_Token:
        fmt.printfln("%s [line %d]", e.msg, e.token.line)
    case parser.Unexpected_Token:
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
    env: ^Env,
    expr: ast.Expr,
    allocator := context.allocator,
) -> (
    val: tok.Literal,
    err: InterpretorError,
) {
    val, err = evaluate_expr(env, expr, context.temp_allocator)
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
    env: ^Env,
    expr: ast.Expr,
    allocator := context.temp_allocator,
) -> (
    val: tok.Literal,
    err: InterpretorError,
) {
    switch t in expr {
    case ^ast.Unary:
        val = evaluate_unary(env, t) or_return
    case ^ast.Binary:
        val = eval_binary(env, t) or_return
        if str, ok := val.(string); ok do val = strings.clone(str, allocator)
    case ^ast.Grouping:
        val, err = evaluate_expr(env, t.expr)
    case ^ast.Literal_Expr:
        val = t.literal
    case ^ast.Variable:
        val = get_variable(env, t.lexeme) or_return
    case ^ast.Logic_Expr:
        left := evaluate_expr(env, t.left_expr) or_return
        if t.operator.type == .OR {
            if is_truthy(left) do return left, nil
        } else {
            if is_truthy(left) do return left, nil
        }

        val = evaluate_expr(env, t.right_expr) or_return

    case ^ast.Assignment:
        val = evaluate_expr(env, t.value) or_return
        ok := set_variable(env, t.identifier.lexeme, val)
        if !ok {
            return val, UndefinedVar{name = t.identifier.lexeme, line = t.identifier.line}
        }
    }

    return val, nil
}

evaluate_unary :: proc(env: ^Env, expr: ^ast.Unary) -> (val: tok.Literal, err: InterpretorError) {
    right := evaluate_expr(env, expr.expr) or_return

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

is_truthy :: proc(val: tok.Literal) -> bool {
    v_bool, ok := val.(bool)
    if val == nil || (ok && !v_bool) {
        return false
    }
    return true
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

eval_binary :: proc(env: ^Env, expr: ^ast.Binary) -> (val: tok.Literal, err: InterpretorError) {
    left := evaluate_expr(env, expr.left_expr) or_return
    right := evaluate_expr(env, expr.right_expr) or_return

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

get_variable :: proc(env: ^Env, variable_name: string) -> (tok.Literal, InterpretorError) {
    #reverse for scope in env.scopes {
        if v, ok := scope[variable_name]; ok {
            return v, nil
        }
    }

    return nil, UndefinedVar{name = variable_name}
}

set_variable :: proc(env: ^Env, variable_name: string, val: tok.Literal) -> bool {
    #reverse for &scope in env.scopes {
        if _, ok := scope[variable_name]; ok {
            scope[variable_name] = val
            return true
        }
    }
    return false
}

env_init :: proc() -> Env {
    return Env{scopes = {map[string]tok.Literal{}}}
}
// clean up allocations for global environment
cleanup :: proc(env: ^Env) {
    length := len(env.scopes)
    for i := 0; i < length; i += 1 {
        cleanup_variables(env)
    }
    delete(env.scopes)
}

@(private = "file")
cleanup_variables :: proc(env: ^Env) {
    if len(env.scopes) > 0 {
        m := pop(&env.scopes)
        for k, v in m {
            delete(k)
            if s, ok := v.(string); ok do delete(s)
        }
        delete(m)
    }
}
