const std = @import("std");
const chunk = @import("chunk.zig");
const values = @import("value.zig");
const util = @import("util.zig");
const Config = @import("config.zig");
const debug = @import("debug.zig");
const Scanner = @import("scanner.zig");
const compiler = @import("compile.zig");

const InterpretResult = enum { INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR };

pub const VM = struct {
    chunk: chunk.Chunk,
    ip: []u8,
    stack: std.ArrayList(values.Value),

    pub fn init(allocator: std.mem.Allocator) !VM {
        var vm = VM{
            .chunk = try chunk.Chunk.init(allocator),
            .ip = undefined,
            .stack = std.ArrayList(values.Value).init(allocator),
        };

        vm.ip = vm.chunk.code.items;
        return vm;
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
    }

    pub fn interpret(self: *VM, alloc: std.mem.Allocator, source: []const u8) InterpretResult {
        self.stack.clearRetainingCapacity();

        if (!compiler.compile(source, &self.chunk)) {
            self.chunk.deinit();
            return .INTERPRET_COMPILE_ERROR;
        }
        self.ip = self.chunk.code.items;

        const result = self.run() catch |err| {
            std.debug.print("Error: {}\n", .{err});
            return .INTERPRET_RUNTIME_ERROR;
        };
        _ = alloc;

        return result;
    }

    fn run(self: *VM) !InterpretResult {
        const stdout = std.io.getStdOut().writer();
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const alloc = gpa.allocator();
        _ = alloc;

        while (self.ip.len > 0) {
            const off = self.offset();
            const next = self.readByte();

            const instruction: chunk.opCode = std.meta.intToEnum(chunk.opCode, next) catch |err| {
                std.debug.print("Error: {} offset: {} instruction: {}\n", .{ err, self.offset(), next });
                return .INTERPRET_COMPILE_ERROR;
            };

            if (util.config.debug) {
                _ = debug.disassembleInstruction(off, next, &self.chunk);
                for (self.stack.items) |v| {
                    switch (v) {
                        .NUMBER => std.debug.print("                [ {d:.2} ]\n", .{v.NUMBER}),
                        else => {},
                    }
                }
            }

            try switch (instruction) {
                .OP_CONSTANT => {
                    const val = self.readConstant();
                    try self.stack.append(val);
                },
                .OP_CONSTANT_LONG => {
                    const val = self.readConstantLong();
                    try self.stack.append(val);
                },
                .OP_RETURN => {
                    std.debug.assert(self.stack.items.len > 0);
                    try values.printValue(self.stack.pop());
                    try stdout.print("\n", .{});
                    break;
                },
                .OP_NEGATE => self.negate(),
                .OP_ADD => self.add(),
                .OP_SUBTRACT => self.subtract(),
                .OP_MULTIPLY => self.multiply(),
                .OP_DIVIDE => self.divide(),
                .OP_NOT => self.stack.append(values.Value{ .BOOL = isFalsey(self.stack.pop()) }),
                .OP_FALSE => self.stack.append(values.Value{ .BOOL = false }),
                .OP_EQUAL => self.equal(),
                .OP_GREATER => self.greater(),
                .OP_LESS => self.less(),
                .OP_TRUE => self.stack.append(values.Value{ .BOOL = true }),
                .OP_NIL => self.stack.append(values.Value.NIL),
            };
        }

        return .INTERPRET_OK;
    }

    fn runtimeError(self: *VM, format: []const u8, args: anytype) void {
        const ArgsType = @TypeOf(args);
        const args_type_info = @typeInfo(ArgsType);

        if (args_type_info != .@"struct") {
            @compileError("expected tuple or struct argument found " ++ @typeName(ArgsType));
        }

        std.debug.print(format, args);
        std.debug.print("\n", .{});

        const line = self.chunk.getLine(self.offset());
        std.debug.print("[line {d} in script\n]", line);
    }

    fn equal(self: *VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();

        try self.stack.append(values.Value{ .BOOL = std.meta.eql(a, b) });
    }

    fn greater(self: *VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();

        std.debug.assert(@as(values.ValueTypes, a) == .NUMBER);
        std.debug.assert(@as(values.ValueTypes, b) == .NUMBER);

        try self.stack.append(values.Value{ .BOOL = a.NUMBER > b.NUMBER });
    }

    fn less(self: *VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();

        std.debug.assert(@as(values.ValueTypes, a) == .NUMBER);
        std.debug.assert(@as(values.ValueTypes, b) == .NUMBER);

        try self.stack.append(values.Value{ .BOOL = a.NUMBER < b.NUMBER });
    }

    fn negate(self: *VM) !void {
        const val = self.peek();
        switch (val.*) {
            .NUMBER => val.* = values.Value{ .NUMBER = -val.NUMBER },
            .BOOL => val.* = values.Value{ .BOOL = !val.BOOL },
            // runtime error?
            else => {},
        }
    }

    fn add(self: *VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();

        std.debug.assert(@as(values.ValueTypes, a) == .NUMBER);
        std.debug.assert(@as(values.ValueTypes, b) == .NUMBER);

        try self.stack.append(values.Value{ .NUMBER = a.NUMBER + b.NUMBER });
    }

    fn subtract(self: *VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();

        std.debug.assert(@as(values.ValueTypes, a) == .NUMBER);
        std.debug.assert(@as(values.ValueTypes, b) == .NUMBER);

        try self.stack.append(values.Value{ .NUMBER = a.NUMBER - b.NUMBER });
    }

    fn multiply(self: *VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();

        std.debug.assert(@as(values.ValueTypes, a) == .NUMBER);
        std.debug.assert(@as(values.ValueTypes, b) == .NUMBER);

        try self.stack.append(values.Value{ .NUMBER = a.NUMBER * b.NUMBER });
    }

    fn divide(self: *VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();

        std.debug.assert(@as(values.ValueTypes, a) == .NUMBER);
        std.debug.assert(@as(values.ValueTypes, b) == .NUMBER);

        try self.stack.append(values.Value{ .NUMBER = a.NUMBER / b.NUMBER });
    }

    fn peek(self: *VM) *values.Value {
        return &self.stack.items[self.stack.items.len - 1];
    }

    fn readByte(self: *VM) u8 {
        const byte = self.ip[0];
        self.ip = self.ip[1..];
        return byte;
    }

    fn offset(self: *VM) usize {
        return self.chunk.code.items.len - self.ip.len;
    }

    fn readConstant(self: *VM) values.Value {
        return self.chunk.constants.items[self.readByte()];
    }

    fn getConstant(self: *VM, idx: usize) values.Value {
        return self.chunk.constants.items[idx];
    }

    fn readConstantLong(self: *VM) values.Value {
        const buf = [3]u8{ self.readByte(), self.readByte(), self.readByte() };
        const idx = util.numFromBytes(buf);
        const val = self.getConstant(idx);
        return val;
    }
};

fn isFalsey(val: values.Value) bool {
    return switch (val) {
        .BOOL => !val.BOOL,
        .NIL => true,
        else => false,
    };
}
