const std = @import("std");

const Config = @This();

debug: bool = false,

pub fn load(allocator: std.mem.Allocator) !Config {
    var args = std.process.args();
    var config = Config{};

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--debug")) {
            config.debug = true;
        }
    }
    _ = allocator;
    return config;
}

pub fn default() Config {
    return Config{};
}
