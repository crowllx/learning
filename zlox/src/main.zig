const std = @import("std");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const vm = @import("vm.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    var testChunk = chunk.Chunk.init(allocator);
    defer testChunk.deinit();

    // try testChunk.writeChunk(55, 1);

    for (0..260) |i| {
        const val: f64 = @floatFromInt(i);
        try testChunk.writeConstant(val, 2);
    }
    try testChunk.writeChunk(@intFromEnum(chunk.opCode.OP_RETURN), 1);

    var virtualMachine = vm.VM.init(allocator);
    const res = virtualMachine.interpret(&testChunk);
    _ = res;

    // debug.disassembleChunk(&testChunk, "test chunk");

    // try bw.flush(); // don't forget to flush!
}
