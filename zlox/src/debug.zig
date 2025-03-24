const chunk = @import("chunk.zig");
const std = @import("std");
const util = @import("util.zig");
const values = @import("value.zig");

pub fn disassembleChunk(data: *chunk.Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var i: usize = 0;
    var prevLine: usize = 0;
    while (i < data.code.items.len) {
        const instruction = data.code.items[i];
        const line = data.getLine(i);
        if (line != prevLine) {
            std.debug.print("{d:0>4} {d:>4} ", .{ i, line });
        } else {
            std.debug.print("{d:0>4}    | ", .{i});
        }
        const offset = disassembleInstruction(i, instruction, data);
        prevLine = line;
        i += offset;
    }
}

pub fn disassembleInstruction(idx: usize, instruction: u8, data: *chunk.Chunk) usize {
    const op: chunk.opCode = std.meta.intToEnum(chunk.opCode, instruction) catch |err| {
        std.debug.print("Error: {} offset: {} instruction: {}\n", .{ err, idx, instruction });
        return 1;
    };

    const offset = switch (op) {
        .OP_CONSTANT => constInstruction("OP_CONSTANT", data, idx),
        .OP_CONSTANT_LONG => constLongInstruction("OP_CONSTANT_LONG", data, idx),
        else => simpleInstruction(@tagName(op)),
    };

    return offset;
}

fn constInstruction(name: []const u8, data: *chunk.Chunk, offset: usize) usize {
    const constant: usize = data.code.items[offset + 1];
    const val = data.constants.items[constant];
    const str = values.toString(val) catch "";
    std.debug.print("{s:<16}  {s}\n", .{ name, str });
    return 2;
}

fn constLongInstruction(name: []const u8, data: *chunk.Chunk, offset: usize) usize {
    const start = offset + 1;
    const buf = [3]u8{ data.code.items[start], data.code.items[start + 1], data.code.items[start + 2] };
    const constant = util.numFromBytes(buf);
    const val = data.constants.items[constant];
    const str = values.toString(val) catch "";
    std.debug.print("{s:<16}  {d:.2}\n", .{ name, str });
    return 4;
}

fn simpleInstruction(name: []const u8) usize {
    std.debug.print("{s}\n", .{name});
    return 1;
}
