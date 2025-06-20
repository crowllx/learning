const std = @import("std");
const chunk = @import("chunk.zig");

pub const ValueArray = std.ArrayList(Value);

pub const ValueTypes = enum {
    BOOL,
    NUMBER,
    NIL,
    OBJ,
};

pub const Value = union(ValueTypes) {
    BOOL: bool,
    NUMBER: f64,
    NIL: void,
    OBJ: Obj,
};

pub const ObjTypes = enum {
    STRING,
    FUNCTION,
    NATIVE_FUNCTION,
    CLOSURE,
};

pub const Obj = union(ObjTypes) {
    STRING: []u8,
    FUNCTION: Function,
    NATIVE_FUNCTION: NativeFn,
    CLOSURE: Closure,
};

pub const Closure = struct {
    function: Function,
    upvalues: std.ArrayList(*Upvalue),
    upvalue_count: usize,
};

pub fn newClosure(allocator: std.mem.Allocator, function: Function) !Closure {
    return Closure{
        .function = function,
        .upvalues = try std.ArrayList(*Upvalue).initCapacity(allocator, function.upvalue_count),
        .upvalue_count = function.upvalue_count,
    };
}

pub const Upvalue = struct {
    location: *Value,
    next: ?*Upvalue,
    closed: Value,
};

pub fn newUpvalue(slot: *Value) Upvalue {
    return Upvalue{ .location = slot, .next = null, .closed = undefined };
}

pub const NativeFn = *const anyopaque;

pub const FunctionType = enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
};

pub const Function = struct {
    arity: u8,
    byte_code: chunk.Chunk,
    name: []const u8,
    upvalue_count: usize,

    pub fn new(allocator: std.mem.Allocator) !Function {
        return Function{
            .arity = 0,
            .byte_code = try chunk.Chunk.init(allocator),
            .name = undefined,
            .upvalue_count = 0,
        };
    }

    pub fn newFrom(dst: *chunk.Chunk) Function {
        return Function{
            .arity = 0,
            .byte_code = dst,
            .name = undefined,
        };
    }

    pub fn deinit(self: *Function) void {
        self.byte_code.deinit();
    }
};

pub fn newString(str: []u8) Value {
    return Value{ .OBJ = Obj{ .STRING = str } };
}

pub fn printObject(object: Obj) void {
    const stdout = std.io.getStdOut().writer();
    switch (object) {
        .STRING => stdout.print("{s}", .{object.STRING}) catch unreachable,
        .FUNCTION => stdout.print("<fn {s}>", .{object.FUNCTION.name}) catch unreachable,
        .NATIVE_FUNCTION => stdout.print("<native fn>", .{}) catch unreachable,
        .CLOSURE => stdout.print("<fn {s}>", .{object.CLOSURE.function.name}) catch unreachable,
    }
}

pub fn objToString(obj: Obj) []const u8 {
    return switch (obj) {
        .FUNCTION => "",
        .STRING => obj.STRING,
        .NATIVE_FUNCTION => "",
        .CLOSURE => "",
    };
}

pub fn toString(allocator: std.mem.Allocator, val: Value) []const u8 {
    return switch (val) {
        .NUMBER => std.fmt.allocPrint(allocator, "{d:.2}", .{val.NUMBER}) catch "",
        .BOOL => std.fmt.allocPrint(allocator, "{}", .{val.BOOL}) catch "",
        .NIL => "nil",
        .OBJ => std.fmt.allocPrint(allocator, "{s}", .{objToString(val.OBJ)}) catch "",
    };
}
pub fn printValue(val: Value) !void {
    const stdout = std.io.getStdOut().writer();
    try switch (val) {
        .NUMBER => stdout.print("{d:.2}", .{val.NUMBER}),
        .BOOL => stdout.print("{}", .{val.BOOL}),
        .NIL => stdout.print("nil", .{}),
        .OBJ => printObject(val.OBJ),
        // else => {},
    };
}
