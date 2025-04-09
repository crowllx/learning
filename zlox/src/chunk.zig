const std = @import("std");
const values = @import("value.zig");
const util = @import("util.zig");

pub const opCode = enum(u8) {
    OP_CONSTANT,
    OP_CONSTANT_LONG,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_LOCAL,
    OP_GET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_SET_LOCAL,
    OP_SET_GLOBAL,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_RETURN,
    OP_PRINT,
    OP_JUMP_IF_FALSE,
};

const LineInfo = struct {
    line: u16,
    instructionCount: u16,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: values.ValueArray,
    lines: std.ArrayList(LineInfo),

    pub fn init(allocator: std.mem.Allocator) !Chunk {
        var c = Chunk{
            .code = std.ArrayList(u8).init(allocator),
            .constants = values.ValueArray.init(allocator),
            .lines = std.ArrayList(LineInfo).init(allocator),
        };
        try c.lines.append(LineInfo{ .line = 1, .instructionCount = 0 });
        return c;
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

    pub fn writeLong(self: *Chunk, idx: u24, line: u16) !void {
        const bytes: [3]u8 = util.bytesFromNum(idx);
        for (bytes) |elem| {
            try self.writeChunk(elem, line);
        }
    }

    pub fn writeConstant(self: *Chunk, val: values.Value, line: u16) !usize {
        try self.constants.append(val);
        const lastIndex = self.constants.items.len - 1;
        if (lastIndex <= 255) {
            try self.writeChunk(@intFromEnum(opCode.OP_CONSTANT), line);
            try self.writeChunk(@intCast(lastIndex), line);
        } else {
            try self.writeChunk(@intFromEnum(opCode.OP_CONSTANT_LONG), line);
            const idx: u24 = @intCast(lastIndex);
            const bytes: [3]u8 = util.bytesFromNum(idx);
            for (bytes) |element| {
                try self.writeChunk(element, line);
            }
        }

        return lastIndex;
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
