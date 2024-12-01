package tree
import "./parser"
import "./tokenizer"
import "core:fmt"
import "core:strings"

Literal :: tokenizer.Literal

Data :: union {
    ^Func,
    Literal,
}

Env :: struct {
    scope:    map[string]Data,
    prev_env: ^Env,
}

env_init :: proc(prev: ^Env = nil, allocator := context.allocator) -> ^Env {
    context.allocator = allocator
    x := new(Env)
    x.scope = map[string]Data{}
    x.prev_env = prev
    return x
}

env_push :: proc(head: ^Env, new_element: ^Env) -> ^Env {
    new_element.prev_env = head
    return new_element
}

// removes and returns head element
env_pop :: proc(env: ^Env) -> (head: ^Env, element: ^Env) {
    return env.prev_env, env
}


// define new variable in top level scope
declare_variable :: proc(env: ^Env, key: string, val: Data) {
    if key in env.scope {
        env.scope[key] = val
    } else {
        env.scope[strings.clone(key)] = val
    }
}

// assign to variable traversing scopes if not found in top level
set_variable :: proc(env: ^Env, key: string, val: Data) -> bool {
    successful: bool
    env := env
    for !successful {
        if key in env.scope {
            successful = true
            env.scope[key] = val
            break
        }


        if env.prev_env == nil do break
        env = env.prev_env
    }

    return successful
}

//retrieve variable, traversing scopes until found
get_variable :: proc(env: ^Env, key: string) -> (Data, bool) {
    successful: bool
    env := env
    val: Data

    for !successful {
        if key in env.scope {
            successful = true
            val = env.scope[key]
            break
        }

        if env.prev_env == nil do break
        env = env.prev_env
    }

    return val, successful
}

delete_data :: proc(d: Data) {
    switch var in d {
    case Literal:
    // if s, ok := var.(string); ok do delete(s)
    case ^Func:
        if var.name != "clock" {
            for id in var.params {
                delete(id)
            }
            delete(var.params)
            delete(var.name)
            fmt.eprintfln("closure %v", var.closure)
            tmp := var.closure
            for tmp.prev_env != nil {
                tmp = cleanup_env(var.closure)
            }

            parser.statement_destroy(var.body)
            free(var)
        }
    }
}

copy_closure :: proc(env: ^Env) -> ^Env {
    if env.prev_env == nil do return env

    buf: [dynamic]^Env
    env := env
    for env.prev_env != nil {
        append(&buf, env)
        env = env.prev_env
    }
    defer delete(buf)

    head := env
    #reverse for env_ptr in buf {
        next := new(Env)
        next.prev_env = head
        for k, v in env_ptr.scope {
            map_insert(&next.scope, strings.clone(k), v)
        }
        head = next
    }
    return head
}

cleanup_env :: proc(env: ^Env) -> ^Env {
    // fmt.eprintfln("CLEANING: %v", env)
    for k, v in env.scope {
        delete(k)
        delete_data(v)
    }
    ret := env.prev_env
    delete(env.scope)
    free(env)
    return ret
}

cleanup :: proc(env: ^Env) {
    env := env
    for env != nil {
        env = cleanup_env(env)
    }
}
