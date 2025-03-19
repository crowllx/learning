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
                    std.debug.print("          [ {d:.2} ]\n", .{v});
                }
            }

            switch (instruction) {
                .OP_CONSTANT => {
                    const val = self.readConstant();
                    try self.stack.append(val);
                },
                .OP_CONSTANT_LONG => {
                    const val = self.readConstantLong();
                    try self.stack.append(val);
                },
                .OP_NEGATE => {
                    self.stack.items[self.stack.items.len - 1] = -self.stack.items[self.stack.items.len - 1];
                },
                .OP_RETURN => {
                    std.debug.assert(self.stack.items.len > 0);
                    try values.printValue(self.stack.pop());
                    try stdout.print("\n", .{});
                    break;
                },
                .OP_ADD => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    try self.stack.append(a + b);
                },
                .OP_SUBTRACT => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    try self.stack.append(a - b);
                },
                .OP_MULTIPLY => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    try self.stack.append(a * b);
                },
                .OP_DIVIDE => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    try self.stack.append(a / b);
                },
            }
        }

        return .INTERPRET_OK;
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
