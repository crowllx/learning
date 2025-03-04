const std = @import("std");
const values = @import("value.zig");
const util = @import("util.zig");

pub const opCode = enum(u8) {
    OP_RETURN,
    OP_CONSTANT,
    OP_CONSTANT_LONG,
};

const LineInfo = struct {
    line: u16,
    instructionCount: u16,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: values.ValueArray,
    lines: std.ArrayList(LineInfo),

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return Chunk{ .code = std.ArrayList(u8).init(allocator), .constants = values.ValueArray.init(allocator), .lines = std.ArrayList(LineInfo).init(allocator) };
        // chunk.lines.append(LineInfo{ .line = 1, .instructionCount = 0 });
        // return chunk;
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn writeChunk(self: *Chunk, byte: u8, line: u16) !void {
        try self.code.append(byte);
        if (self.lines.items.len < line) {
            try self.lines.append(LineInfo{ .line = line, .instructionCount = 0 });
        } else {
            std.debug.assert(self.lines.items.len > 0);
            self.lines.items[self.lines.items.len - 1].instructionCount += 1;
        }
    }

    pub fn writeConstant(self: *Chunk, val: values.Value, line: u16) !void {
        try self.constants.append(val);
        const lastIndex = self.constants.items.len - 1;
        if (lastIndex <= 255) {
            try self.writeChunk(@intFromEnum(opCode.OP_CONSTANT), line);
            try self.writeChunk(@intCast(lastIndex), line);
        } else {
            try self.writeChunk(@intFromEnum(opCode.OP_CONSTANT_LONG), line);
            const idx: u24 = @intCast(lastIndex);
            // try self.writeChunk(@intCast(idx & 0xff), line);
            // try self.writeChunk(@intCast((idx >> 8) & 0xff), line);
            // try self.writeChunk(@intCast((idx >> 16) & 0xff), line);
            const bytes: [3]u8 = util.bytesFromNum(idx);
            for (bytes) |element| {
                try self.writeChunk(element, line);
            }
        }
    }

    pub fn getLine(self: *Chunk, index: usize) u16 {
        var count: usize = 0;

        for (self.lines.items) |line| {
            count += line.instructionCount;
            if (count >= index) return line.line;
        }
        return 0;
    }

    pub fn addConstant(self: *Chunk, val: values.Value) !usize {
        try self.constants.append(val);
        return self.constants.items.len - 1;
    }
};
