const std = @import("std");
const chunk = @import("chunk.zig");

pub const ValueArray = std.ArrayList(Value);

pub const ValueTypes = enum {
    BOOL,
    NUMBER,
    NIL,
    STRING,
};

pub const Value = union(ValueTypes) {
    BOOL: bool,
    NUMBER: f64,
    NIL: void,
    STRING: []u8,
};

pub const ObjTypes = enum {
    STRING,
    FUNCTION,
};

pub const Obj = union(ObjTypes) {
    STRING: Value,
    FUNCTION: Function,
};

pub const FunctionType = enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
};

pub const Function = struct {
    arity: u8,
    byte_code: *chunk.Chunk,
    name: []const u8,

    pub fn new(allocator: std.mem.Allocator) Function {
        return Function{
            .arity = 0,
            .byte_code = &chunk.Chunk.init(allocator),
            .name = undefined,
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

pub fn printObject(object: Obj) void {
    const stdout = std.io.getStdOut().writer();
    switch (object) {
        .STRING => printValue(object.STRING),
        .FUNCTION => stdout.print("<fn {s}>", .{object.FUNCTION.name}) catch unreachable,
    }
}

pub fn toString(allocator: std.mem.Allocator, val: Value) []const u8 {
    return switch (val) {
        .NUMBER => std.fmt.allocPrint(allocator, "{d:.2}", .{val.NUMBER}) catch "",
        .BOOL => std.fmt.allocPrint(allocator, "{}", .{val.BOOL}) catch "",
        .NIL => "nil",
        .STRING => val.STRING,
    };
}
pub fn printValue(val: Value) !void {
    const stdout = std.io.getStdOut().writer();
    try switch (val) {
        .NUMBER => stdout.print("{d:.2}", .{val.NUMBER}),
        .BOOL => stdout.print("{}", .{val.BOOL}),
        .NIL => stdout.print("nil", .{}),
        .STRING => stdout.print("{s}", .{val.STRING}),
        // else => {},
    };
}
