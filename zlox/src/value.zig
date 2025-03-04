const std = @import("std");

pub const Value = f64;
pub const ValueArray = std.ArrayList(f64);

pub fn printValue(val: Value) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d:.2}", .{val});
}
