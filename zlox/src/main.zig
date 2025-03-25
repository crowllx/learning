const std = @import("std");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const vm = @import("vm.zig");
const Scanner = @import("scanner.zig");
const Config = @import("config.zig");
const util = @import("util.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    defer _ = gpa.detectLeaks();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    Config.load(&util.config);

    if (args.len >= 2) {
        std.debug.print("{s}\n", .{args[1]});
        try runFile(allocator, args[1]);
    } else {
        try std.io.getStdOut().writer().print("not enough args.\n", .{});
    }
}

fn repl() void {
    var buf: [1024]u8 = undefined;
    var stdin = std.io.getStdIn().reader();
    var stdout = std.io.bufferedWriter(std.io.getStdIn().writer());

    try stdout.write("> ");

    while (stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |line| {
        _ = line;
    }
}

fn runFile(allocator: std.mem.Allocator, file_path: []const u8) !void {
    const file_content = try std.fs.cwd().readFileAlloc(allocator, file_path, std.math.maxInt(usize));
    defer allocator.free(file_content);
    var virtual_machine = try vm.VM.init(allocator);
    defer virtual_machine.deinit();
    const result = virtual_machine.interpret(allocator, file_content);
    _ = result;
}
