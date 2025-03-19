const std = @import("std");

const Config = @This();

debug: bool = false,

pub fn load(config: *Config) void {
    var args = std.process.args();

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--debug")) {
            config.debug = true;
        }
    }
}

pub fn default() Config {
    return Config{};
}
