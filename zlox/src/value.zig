const std = @import("std");

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
};

pub const Obj = union(ObjTypes) {
    STRING: Value,
};

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
