const std = @import("std");

const Config = @This();

pub fn bytesFromNum(num: u24) [3]u8 {
    var buf = std.mem.zeroes([3]u8);
    buf[0] = @intCast(num & 0xff);
    buf[1] = @intCast((num >> 8) & 0xff);
    buf[2] = @intCast((num >> 16) & 0xff);
    return buf;
}

pub fn numFromBytes(bytes: [3]u8) usize {
    var num: usize = @as(usize, bytes[2]) << 16;
    num = num | (@as(usize, bytes[1]) << 8);
    num = num | @as(usize, bytes[0]);
    return num;
}
