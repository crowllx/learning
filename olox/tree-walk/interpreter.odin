package tree

import "ast"
import "core:strings"
import tok "tokenizer"


EvaluationError :: struct {
    msg: string,
}

TypeError :: struct {
    left:  tok.Literal,
    right: tok.Literal,
    msg:   string,
}

InterpretorError :: union {
    EvaluationError,
    TypeError,
}

evaluate_expr :: proc(expr: ast.Expression) -> (val: tok.Literal, err: InterpretorError) {
    switch t in expr {
    case ^ast.Unary:
        val = evaluate_unary(t) or_return
    case ^ast.Binary:
        val = eval_binary(t) or_return
    case ^ast.Grouping:
        val, err = evaluate_expr(t.expr)
    case ^ast.LiteralExpr:
        val = t.literal
    }
    return val, nil
}

evaluate_unary :: proc(expr: ^ast.Unary) -> (val: tok.Literal, err: InterpretorError) {
    right := evaluate_expr(expr.expr) or_return

    #partial switch expr.operator.type {
    case .MINUS:
        val = -right.(f64)
    case .BANG:
        if right == nil || right == false {
            val = true
        } else {
            val = false
        }
    case:
        return nil, EvaluationError{msg = "Unexpected Unary Operator"}
    }

    return val, nil
}

check_number :: proc(left: tok.Literal, right: tok.Literal) -> InterpretorError {
    _, left_ok := left.(f64)
    _, right_ok := right.(f64)
    if !left_ok || !right_ok {
        return TypeError {
            left = left,
            right = right,
            msg = "Invalid types for arithmetic expression",
        }
    }
    return nil
}

check_string :: proc(left: tok.Literal, right: tok.Literal) -> InterpretorError {
    _, left_ok := left.(string)
    _, right_ok := right.(string)
    if !left_ok || !right_ok {
        return TypeError {
            left = left,
            right = right,
            msg = "Invalid types for string concatenation",
        }
    }
    return nil
}

eval_binary :: proc(expr: ^ast.Binary) -> (val: tok.Literal, err: InterpretorError) {
    left := evaluate_expr(expr.left_expr) or_return
    right := evaluate_expr(expr.right_expr) or_return

    #partial switch expr.operator.type {
    case .MINUS:
        check_number(left, right) or_return
        val = left.(f64) - right.(f64)
    case .SLASH:
        check_number(left, right) or_return
        val = left.(f64) / right.(f64)
    case .STAR:
        check_number(left, right) or_return
        val = left.(f64) * right.(f64)
    case .PLUS:
        #partial switch t in left {
        case f64:
            check_number(left, right) or_return
            val = left.(f64) + right.(f64)
        case string:
            check_string(left, right) or_return
            val = strings.concatenate({left.(string), right.(string)})
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
        return nil, EvaluationError{msg = "Unexpected operator in binary expression"}
    }
    return val, nil
}
