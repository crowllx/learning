const std = @import("std");

pub const ValueArray = std.ArrayList(Value);

pub const ValueTypes = enum {
    BOOL,
    NUMBER,
    NIL,
};

pub const Value = union(ValueTypes) {
    BOOL: bool,
    NUMBER: f64,
    NIL: void,
};

pub fn toString(val: Value) ![]const u8 {
    switch (val) {
        .NUMBER => {
            var buf: [256]u8 = undefined;
            return try std.fmt.bufPrint(&buf, "{d:.2}", .{val.NUMBER});
        },
        .BOOL => return "bool",
        .NIL => return "nil",
    }
}
pub fn printValue(val: Value) !void {
    const stdout = std.io.getStdOut().writer();
    switch (val) {
        .NUMBER => try stdout.print("{d:.2}", .{val.NUMBER}),
        else => {},
    }
}
