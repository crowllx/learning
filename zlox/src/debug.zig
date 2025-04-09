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
        .OP_SET_LOCAL, .OP_GET_LOCAL => byteInstruction(@tagName(op), data, idx),
        else => simpleInstruction(@tagName(op)),
    };

    return offset;
}

fn byteInstruction(name: []const u8, data: *chunk.Chunk, offset: usize) usize {
    const slot = data.code.items[offset + 1];
    std.debug.print("{s:<16} {d:>4}", .{ name, slot });
    return offset + 2;
}

fn constInstruction(name: []const u8, data: *chunk.Chunk, offset: usize) usize {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const constant: usize = data.code.items[offset + 1];
    const val = data.constants.items[constant];
    const str = values.toString(allocator, val);
    std.debug.print("{s:<16}  {s}\n", .{ name, str });
    return 2;
}

fn constLongInstruction(name: []const u8, data: *chunk.Chunk, offset: usize) usize {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const start = offset + 1;
    const buf = [3]u8{ data.code.items[start], data.code.items[start + 1], data.code.items[start + 2] };
    const constant = util.numFromBytes(buf);
    const val = data.constants.items[constant];
    const str = values.toString(allocator, val);
    std.debug.print("{s:<16}  {d:.2}\n", .{ name, str });
    return 4;
}

fn simpleInstruction(name: []const u8) usize {
    std.debug.print("{s}\n", .{name});
    return 1;
}
