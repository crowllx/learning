package tree
import "ast"
import "core:fmt"
import "core:log"
import "core:mem"
import "core:strconv"
import "core:strings"
import "core:time"
import "parser"
import tok "tokenizer"


closure :: map[string]tok.Literal
Env :: struct {
    scopes:    [dynamic]map[string]tok.Literal,
    global:    ^map[string]tok.Literal,
    functions: map[string]Func,
}

Named_Entity :: union {
    Func,
    tok.Literal,
}

Func :: struct {
    arity:   int,
    params:  []string,
    name:    string,
    body:    ast.Block,
    call:    proc(env: ^Env, f: ^Func, args: []tok.Literal) -> (tok.Literal, InterpretorError),
    closure: closure,
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
Call_Error :: struct {
    callee: tok.Literal,
    msg:    string,
    line:   int,
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
    Call_Error,
    ast.Return_Stmt,
}

new_call_error :: proc(
    msg: string = "Can only call functiosn and classes.",
    callee: tok.Literal,
    line: int,
) -> Call_Error {
    return Call_Error{msg = msg, callee = callee, line = line}
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
        val = evaluate_expr(env, v.expr) or_return
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
        c := evaluate_expr(env, v.condition) or_return
        if c == false || c == nil {
            val, err = execute_stmt(env, v.else_stmt^)
        } else {
            val, err = execute_stmt(env, v.then_stmt^)
        }

    case ast.While_Stmt:
        condition := evaluate_expr(env, v.condition) or_return
        new_scope: map[string]tok.Literal
        append(&env.scopes, new_scope)
        defer cleanup_variables(env)

        for is_truthy(condition) {
            val = execute_block(env, v.body^.(ast.Block)) or_return
            condition = evaluate_expr(env, v.condition) or_return
        }


        for k, v in new_scope {
            delete(k)
            if s, ok := v.(string); ok {
                delete(s)
            }
        }

    case ast.Block:
        val, err = execute_block(env, v)
    case ast.Function:
        params: [dynamic]string

        for id in v.params {
            append(&params, strings.clone(id))
        }
        body_ptr := cast(ast.Stmt)v.body

        closure: closure
        if len(env.scopes) > 1 {
            closure = env.scopes[len(env.scopes) - 1]
        } else {
            closure = map[string]tok.Literal{}
        }

        func := Func {
            arity   = len(v.params),
            name    = strings.clone(v.name),
            params  = params[:],
            body    = ast.copy_stmt(&body_ptr).(ast.Block),
            closure = closure,
        }
        func.call = call_func
        map_insert(&env.functions, func.name, func)
        map_insert(&env.scopes[len(env.scopes) - 1], strings.clone(func.name), strings.clone(func.name))

    // return stmt as an error to indicate to the interpretor to escape the current block
    case ast.Return_Stmt:
        val = evaluate_expr(env, v.value) or_return
        err = v
    }
    // if s, ok := val.(string); ok do delete(s)
    return val, err
}

// expects a new scope to added to the stack prior to calling,
// and caller must cleanup that scope
execute_block :: proc(env: ^Env, block: ast.Block) -> (val: tok.Literal, err: InterpretorError) {
    for statement in block.stmts {
        tmp_val, tmp_err := execute_stmt(env, statement)
        if tmp_err != nil {
            if _, ok := tmp_err.(ast.Return_Stmt); ok {
                val = tmp_val
                err = tmp_err
                break
            } else {
                return nil, tmp_err
            }

        }
    }
    return val, err
}

report_error :: proc(err: InterpretorError) {
    #partial switch e in err {
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
    case Call_Error:
        fmt.eprintfln("%s (%s) [line %d]", e.msg, e.callee, e.line)
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
    case ^ast.Call:
        callee := evaluate_expr(env, t.callee) or_return
        key := callee.(string)
        args: [dynamic]tok.Literal
        defer delete(args)
        // defer delete(key)

        for a in t.args {
            v := evaluate_expr(env, a) or_return
            append(&args, v)
        }

        function, ok := env.functions[key]

        if !ok {
            return nil, new_call_error(callee = callee, line = t.paren.line)
        }
        val, err = function.call(env, &function, args[:])
        if _, ok := err.(ast.Return_Stmt); ok do err = nil
    }

    return val, err
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
        if current_val, ok := scope[variable_name]; ok {
            if s, is_string := current_val.(string); is_string do delete(s)
            map_insert(&scope, variable_name, val)
            scope[variable_name] = val
            return true
        }
    }
    return false
}
args_error :: proc(num_args, num_params, line: int) -> Call_Error {
    buf: [4]byte
    err_msg := strings.concatenate(
        {
            "Incorrect number of args. Expected ",
            strconv.itoa(buf[:], num_params),
            " got ",
            strconv.itoa(buf[:], num_args),
        },
    )
    return Call_Error{line = line, msg = err_msg}
}

call :: proc {
    call_func,
}

call_func :: proc(
    env: ^Env,
    func: ^Func,
    args: []tok.Literal,
) -> (
    ret_val: tok.Literal,
    err: InterpretorError,
) {
    append(&env.scopes, func.closure)
    defer pop(&env.scopes)
    append(&env.scopes, map[string]tok.Literal{})
    defer cleanup_variables(env)

    if len(args) < func^.arity {
        return nil, Call_Error {
            line = 13,
            msg = "Not Enough arguments to function call",
            callee = func.name,
        }
    }

    for id, index in func.params {
        val: tok.Literal
        if s, ok := args[index].(string); ok {
            val = strings.clone(s)
        } else {
            val = args[index]
        }
        map_insert(&env.scopes[len(env.scopes) - 1], strings.clone(id), val)
    }


    ret_val, err = execute_block(env, func.body)
    return ret_val, err
}


// if i can find a better way to deallocate used keys at the end of sessions i could
// avoid cloning the string literal "clock" and reduce heap allocations
env_init :: proc() -> Env {
    env := Env {
        scopes = {map[string]tok.Literal{}},
    }
    env.global = &env.scopes[0]
    key := strings.clone("clock")
    map_insert(&env.functions, key, clock)
    map_insert(env.global, key, strings.clone(key))
    return env
}

// clean up allocations for global environment
cleanup :: proc(env: ^Env) {
    for k, v in env.functions {
        if k != "clock" {
            for name in v.params {
                delete(name)
            }
            delete(v.params)

            delete(v.name)
            parser.statement_destroy(v.body)
        }
    }
    length := len(env.scopes)
    for i := 0; i < length; i += 1 {
        cleanup_variables(env)
    }
    delete(env.scopes)
    delete(env.functions)
}

@(private = "file")
cleanup_variables :: proc(env: ^Env) {
    if len(env.scopes) > 0 {
        m := pop(&env.scopes)
        for k, v in m {
            if s, ok := v.(string); ok do delete(s)
            delete(k)
        }
        delete(m)
    }
}


// native functions

clock :: Func {
    arity = 0,
    name = "clock",
    call = proc(env: ^Env, f: ^Func, args: []tok.Literal) -> (tok.Literal, InterpretorError) {
        return f64(time.time_to_unix(time.now())), nil
    },
}
