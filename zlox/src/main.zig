const std = @import("std");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const vm = @import("vm.zig");
const Scanner = @import("scanner.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // var testChunk = chunk.Chunk.init(allocator);
    // defer testChunk.deinit();

    // try testChunk.writeConstant(2, 123);
    // try testChunk.writeConstant(3, 123);
    // try testChunk.writeChunk(@intFromEnum(chunk.opCode.OP_ADD), 123);
    // try testChunk.writeConstant(3, 123);
    // try testChunk.writeChunk(@intFromEnum(chunk.opCode.OP_MULTIPLY), 123);
    // try testChunk.writeChunk(@intFromEnum(chunk.opCode.OP_NEGATE), 123);
    // try testChunk.writeChunk(@intFromEnum(chunk.opCode.OP_RETURN), 123);

    // var virtualMachine = vm.VM.init(allocator);
    // const res = virtualMachine.interpret(&testChunk);
    // _ = res;

    var args = std.process.args();
    _ = args.skip();

    if (args.next()) |file_path| {
        std.debug.print("{s}\n", .{file_path});
        try runFile(allocator, file_path);
    } else {
        std.process.exit(50);
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

    var scanner = Scanner.new(file_content);

    var token: Scanner.Token = scanner.scanToken();

    while (token.type != .TOKEN_EOF) : (token = scanner.scanToken()) {
        token.print();
    }
}
