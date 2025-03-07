const chunk = @import("chunk.zig");
const std = @import("std");
const util = @import("util.zig");

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
        .OP_RETURN => simpleInstruction("OP_RETURN"),
        .OP_CONSTANT => constInstruction("OP_CONSTANT", data, idx),
        .OP_CONSTANT_LONG => constLongInstruction("OP_CONSTANT_LONG", data, idx),
        .OP_NEGATE => simpleInstruction("OP_NEGATE"),
        .OP_ADD => simpleInstruction("OP_ADD"),
        .OP_SUBTRACT => simpleInstruction("OP_SUBSTRACT"),
        .OP_MULTIPLY => simpleInstruction("OP_MULTIPLY"),
        .OP_DIVIDE => simpleInstruction("OP_DIVIDE"),
    };

    return offset;
}

fn constInstruction(name: []const u8, data: *chunk.Chunk, offset: usize) usize {
    const constant: usize = data.code.items[offset + 1];
    std.debug.print("{s:<16}  {d:.2}\n", .{ name, data.constants.items[constant] });
    return 2;
}

fn constLongInstruction(name: []const u8, data: *chunk.Chunk, offset: usize) usize {
    const start = offset + 1;
    // var constant: usize = @as(usize, data.code.items[start + 2]) << 16;
    // constant = constant | @as(usize, data.code.items[start + 1]) << 8;
    // constant = constant | @as(usize, data.code.items[start]);
    const buf = [3]u8{ data.code.items[start], data.code.items[start + 1], data.code.items[start + 2] };
    const constant = util.numFromBytes(buf);

    std.debug.print("{s:<16}  {d:.2}\n", .{ name, data.constants.items[constant] });
    return 4;
}

fn simpleInstruction(name: []const u8) usize {
    std.debug.print("{s}\n", .{name});
    return 1;
}
