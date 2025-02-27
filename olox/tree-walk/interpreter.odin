package tree
import "ast"
import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:strconv"
import "core:strings"
import "core:time"
import "parser"
import tok "tokenizer"


Named_Entity :: union {
    Func,
    tok.Literal,
}

Func :: struct {
    arity:   int,
    params:  []string,
    name:    string,
    body:    ast.Block,
    call:    proc(v: ^Visitor, f: ^Func, args: []Data) -> (Data, Error),
    closure: ^Env,
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

Return_Flag :: enum {
    RETURN,
    CLOSURE,
}

Return_Flags :: bit_set[Return_Flag]

Error :: union {
    EvaluationError,
    TypeError,
    mem.Allocator_Error,
    parser.Unexpected_Token,
    parser.Missing_Token,
    UndefinedVar,
    Call_Error,
    ast.Return_Stmt,
}

IVisitor :: struct {
    // statement evaluating functions
    visit_expr_stmt:    proc(i: ^Visitor, _: ast.Expr_Stmt) -> (Data, Error),
    visit_decl_stmt:    proc(i: ^Visitor, _: ast.Decl) -> (Data, Error),
    visit_if_stmt:      proc(i: ^Visitor, _: ast.If_Stmt) -> (Data, Error),
    visit_while:        proc(i: ^Visitor, _: ast.While_Stmt) -> (Data, Error),
    visit_block:        proc(i: ^Visitor, _: ast.Block) -> (Data, Error),
    visit_function:     proc(i: ^Visitor, _: ast.Function) -> (Data, Error),
    visit_return_stmt:  proc(i: ^Visitor, _: ast.Return_Stmt) -> (Data, Error),

    // expression evaluating functions
    visit_unary:        proc(i: ^Visitor, _: ^ast.Unary) -> (Data, Error),
    visit_binary:       proc(i: ^Visitor, _: ^ast.Binary) -> (Data, Error),
    visit_assignment:   proc(i: ^Visitor, _: ^ast.Assignment) -> (Data, Error),
    visit_call:         proc(i: ^Visitor, _: ^ast.Call) -> (Data, Error),
    visit_logic_expr:   proc(i: ^Visitor, _: ^ast.Logic_Expr) -> (Data, Error),
    visit_grouping:     proc(i: ^Visitor, _: ^ast.Grouping) -> (Data, Error),
    visit_literal_expr: proc(i: ^Visitor, _: ^ast.Literal_Expr) -> (Data, Error),
    visit_variable:     proc(i: ^Visitor, _: ^ast.Variable) -> (Data, Error),
}

Visitor :: struct {
    using visitor: IVisitor,
    env:           ^Env,
    flags:         Return_Flags,
}

interpretor_init :: proc() -> Visitor {
    return Visitor {
        env = env_init(),
        visit_expr_stmt = visit_expr_stmt,
        visit_decl_stmt = visit_decl_stmt,
        visit_if_stmt = visit_if_stmt,
        visit_while = visit_while,
        visit_block = visit_block,
        visit_function = visit_function,
        visit_return_stmt = visit_return_stmt,
        visit_unary = visit_unary,
        visit_binary = visit_binary,
        visit_assignment = visit_assignment,
        visit_call = visit_call,
        visit_logic_expr = visit_logic_expr,
        visit_grouping = visit_grouping,
        visit_literal_expr = visit_literal_expr,
        visit_variable = visit_variable,
    }
}

walk_expr :: proc(v: ^Visitor, expr: ast.Expr) -> (data: Data, err: Error) {
    switch e in expr {
    case ^ast.Call:
        data, err = v.visit_call(v, e)
    case ^ast.Unary:
        data, err = v.visit_unary(v, e)
    case ^ast.Binary:
        data, err = v.visit_binary(v, e)
    case ^ast.Assignment:
        data, err = v.visit_assignment(v, e)
    case ^ast.Logic_Expr:
        data, err = v.visit_logic_expr(v, e)
    case ^ast.Grouping:
        data, err = v.visit_grouping(v, e)
    case ^ast.Literal_Expr:
        data, err = v.visit_literal_expr(v, e)
    case ^ast.Variable:
        data, err = v.visit_variable(v, e)
    }
    switch v in data {
    case ^Func:
    case Literal:
    // if s, ok := v.(string); ok do data = Literal(strings.clone(s))
    }
    return data, err
}

walk_stmt :: proc(v: ^Visitor, stmt: ast.Stmt) -> (Data, Error) {
    switch s in stmt {
    case ast.Expr_Stmt:
        return v.visit_expr_stmt(v, s)
    case ast.Decl:
        return v.visit_decl_stmt(v, s)
    case ast.If_Stmt:
        return v.visit_if_stmt(v, s)
    case ast.While_Stmt:
        return v.visit_while(v, s)
    case ast.Block:
        return v.visit_block(v, s)
    case ast.Function:
        return v.visit_function(v, s)
    case ast.Return_Stmt:
        return v.visit_return_stmt(v, s)
    }
    return nil, nil
}

visit_expr_stmt :: proc(v: ^Visitor, stmt: ast.Expr_Stmt) -> (data: Data, err: Error) {
    data = walk_expr(v, stmt.expr) or_return
    if stmt.type == .PRINT_STMT {
        fmt.println(data)
    }
    return 
}

visit_decl_stmt :: proc(v: ^Visitor, stmt: ast.Decl) -> (data: Data, err: Error) {
    data, err = walk_expr(v, stmt.expr)
    declare_variable(v.env, stmt.id, data)
    return data, err
}

visit_if_stmt :: proc(v: ^Visitor, stmt: ast.If_Stmt) -> (data: Data, err: Error) {
    con := walk_expr(v, stmt.condition) or_return
    if con.(Literal) == false || con == nil {
        data, err = walk_stmt(v, stmt.else_stmt^)
    } else {
        data, err = walk_stmt(v, stmt.then_stmt^)
    }
    return data, err
}

visit_while :: proc(v: ^Visitor, stmt: ast.While_Stmt) -> (data: Data, err: Error) {
    con := walk_expr(v, stmt.condition) or_return

    for is_truthy(con.(Literal)) {
        data, err = walk_stmt(v, stmt.body^)
        con = walk_expr(v, stmt.condition) or_return
        if err != nil {
            break
        }
    }

    return data, err
}
visit_block :: proc(v: ^Visitor, stmt: ast.Block) -> (data: Data, err: Error) {
    v.env = env_init(v.env)
    defer {
        v.env = cleanup_env(v.env)
    }
    return execute_block(v, v.env, stmt)
}

visit_function :: proc(v: ^Visitor, stmt: ast.Function) -> (data: Data, err: Error) {
    params: [dynamic]string

    for id in stmt.params {
        append(&params, strings.clone(id))
    }

    closure := copy_closure(v.env)
    body_ptr := cast(ast.Stmt)stmt.body

    func := Func {
        arity   = len(stmt.params),
        name    = strings.clone(stmt.name),
        params  = params[:],
        body    = ast.copy_stmt(&body_ptr).(ast.Block),
        closure = closure,
        call    = call_func,
    }
    map_insert(&v.env.scope, strings.clone(func.name), new_clone(func))

    v.flags = v.flags | {.CLOSURE}

    return data, err
}

visit_return_stmt :: proc(v: ^Visitor, stmt: ast.Return_Stmt) -> (data: Data, err: Error) {
    data = walk_expr(v, stmt.value) or_return
    if v, ok := data.(^Func); ok {
        data = copy_func(v)
    }
    err = stmt
    return data, err
}

// expression evaluating functions
visit_unary :: proc(i: ^Visitor, expr: ^ast.Unary) -> (data: Data, err: Error) {
    right := walk_expr(i, expr.expr) or_return
    #partial switch expr.operator.type {
    case .MINUS:
        data = Literal(-right.(Literal).(f64))
    case .BANG:
        data = right == nil || right == false

    case:
        return nil, EvaluationError{msg = "Unexpected Unary Operator", line = expr.operator.line}
    }
    return data, err
}
visit_binary :: proc(v: ^Visitor, expr: ^ast.Binary) -> (data: Data, err: Error) {
    left := walk_expr(v, expr.left_expr) or_return
    right := walk_expr(v, expr.right_expr) or_return

    #partial switch expr.operator.type {
    case .MINUS:
        check_number(left.(Literal), right.(Literal), expr.operator.line) or_return
        data = Literal(left.(Literal).(f64) - right.(Literal).(f64))
    case .SLASH:
        check_number(left.(Literal), right.(Literal), expr.operator.line) or_return
        data = Literal(left.(Literal).(f64) / right.(Literal).(f64))
    case .STAR:
        check_number(left.(Literal), right.(Literal), expr.operator.line) or_return
        data = Literal(left.(Literal).(f64) * right.(Literal).(f64))
    case .PLUS:
        left := left.(Literal)
        right := right.(Literal)
        #partial switch t in left {
        case f64:
            check_number(left, right, expr.operator.line) or_return
            data = Literal(left.(f64) + right.(f64))
        case string:
            check_string(left, right, expr.operator.line) or_return
            str := strings.concatenate(
                {left.(string), right.(string)},
                context.temp_allocator,
            ) or_return
            data = Literal(str)
        }
    case .GREATER:
        left := left.(Literal)
        right := right.(Literal)
        data = left.(f64) > right.(f64)
    case .GREATER_EQUAL:
        left := left.(Literal)
        right := right.(Literal)
        data = left.(f64) >= right.(f64)
    case .LESS:
        left := left.(Literal)
        right := right.(Literal)
        data = left.(f64) < right.(f64)
    case .LESS_EQUAL:
        left := left.(Literal)
        right := right.(Literal)
        data = left.(f64) <= right.(f64)
    case .BANG_EQUAL:
        data = left != right
    case .EQUAL_EQUAL:
        data = left == right
    case:
        return nil, EvaluationError {
            msg = "Unexpected operator in binary expression",
            line = expr.operator.line,
        }
    }
    return data, nil

}
visit_assignment :: proc(v: ^Visitor, expr: ^ast.Assignment) -> (data: Data, err: Error) {
    data = walk_expr(v, expr.value) or_return
    ok := set_variable(v.env, expr.identifier.lexeme, data)

    if !ok {
        return data, UndefinedVar{name = expr.identifier.lexeme, line = expr.identifier.line}
    }

    return data, err
}

visit_call :: proc(v: ^Visitor, expr: ^ast.Call) -> (data: Data, err: Error) {
    callee := walk_expr(v, expr.callee) or_return

    func := callee.(^Func)
    args: [dynamic]Data
    defer delete(args)

    for arg in expr.args {
        val := walk_expr(v, arg) or_return
        append(&args, val)
    }

    data, err = func.call(v, func, args[:])
    if _, ok := err.(ast.Return_Stmt); ok do err = nil
    return data, err
}
visit_logic_expr :: proc(v: ^Visitor, expr: ^ast.Logic_Expr) -> (data: Data, err: Error) {
    left := walk_expr(v, expr.left_expr) or_return
    if expr.operator.type == .OR {
        if is_truthy(left.(Literal)) do return left, nil
    } else {
        if is_truthy(left.(Literal)) do return left, nil
    }

    data = walk_expr(v, expr.right_expr) or_return
    return data, err
}

visit_grouping :: proc(v: ^Visitor, expr: ^ast.Grouping) -> (data: Data, err: Error) {
    return walk_expr(v, expr.expr)
}

literal :: ast.Literal_Expr

visit_literal_expr :: proc(i: ^Visitor, expr: ^literal) -> (data: Data, err: Error) {
    return expr.literal, nil
}

visit_variable :: proc(v: ^Visitor, expr: ^ast.Variable) -> (data: Data, err: Error) {
    var, ok := get_variable(v.env, expr.lexeme)
    if !ok {
        return nil, UndefinedVar{name = expr.lexeme, line = expr.line}
    }
    data = var

    return data, err
}
// statement evaluating functions

new_call_error :: proc(
    msg: string = "Can only call functiosn and classes.",
    callee: tok.Literal,
    line: int,
) -> Call_Error {
    return Call_Error{msg = msg, callee = callee, line = line}
}

interpret :: proc(v: ^Visitor, stmts: []ast.Stmt, errs: []parser.Parsing_Error) {
    for s in stmts {
        _, err := walk_stmt(v, s)
        report_error(err)
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

execute_block :: proc(v: ^Visitor, env: ^Env, block: ast.Block) -> (val: Data, err: Error) {
    visitor := v^
    visitor.env = env
    for statement in block.stmts {
        tmp_val, tmp_err := walk_stmt(&visitor, statement)
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

report_error :: proc(err: Error) {
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

@(private = "file")
is_truthy :: proc(val: tok.Literal) -> bool {
    v_bool, ok := val.(bool)
    if val == nil || (ok && !v_bool) {
        return false
    }
    return true
}
check_number :: proc(left: tok.Literal, right: tok.Literal, line: int) -> Error {
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

check_string :: proc(left: tok.Literal, right: tok.Literal, line: int) -> Error {
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

call_func :: proc(v: ^Visitor, func: ^Func, args: []Data) -> (ret_val: Data, err: Error) {

    visitor := v^
    visitor.env = env_init(func.closure)
    defer cleanup_env(visitor.env)


    if len(args) < func^.arity {
        return nil, Call_Error {
            line = 13,
            msg = "Not Enough arguments to function call",
            callee = func.name,
        }
    }

    for id, index in func.params {

        val: Data
        if s, ok := args[index].(Literal).(string); ok {
            val = Literal(strings.clone(s))
        } else {
            val = args[index]
        }
        declare_variable(visitor.env, id, val)
    }

    ret_val, err = execute_block(&visitor, visitor.env, func.body)
    // clean up environment data unless a closure has been captured
    return ret_val, err
}


copy_func :: proc(f: ^Func) -> ^Func {
    x := new(Func)
    x.arity = f.arity
    x.name = strings.clone(f.name)
    x.call = f.call
    x.closure = copy_closure(f.closure)
    params: [dynamic]string
    for p in f.params {
        append(&params, p)
    }
    x.params = params[:]
    body_ptr := cast(ast.Stmt)f.body
    x.body = ast.copy_stmt(&body_ptr).(ast.Block)

    return x
}

// native functions

clock :: Func {
    arity = 0,
    name = "clock",
    call = proc(v: ^Visitor, f: ^Func, args: []Data) -> (Data, Error) {
        return Literal(f64(time.time_to_unix(time.now()))), nil
    },
}
