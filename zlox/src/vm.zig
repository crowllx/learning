const std = @import("std");
const builtin = @import("builtin");
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
    globals: std.StringHashMap(values.Value),
    objects: std.ArrayList(values.Obj),
    allocator: std.mem.Allocator,

    pub fn init(gpa: std.mem.Allocator) !VM {
        var vm = VM{
            .chunk = try chunk.Chunk.init(gpa),
            .ip = undefined,
            .stack = std.ArrayList(values.Value).init(gpa),
            .objects = std.ArrayList(values.Obj).init(gpa),
            .globals = std.StringHashMap(values.Value).init(gpa),
            .allocator = gpa,
        };

        vm.ip = vm.chunk.code.items;
        return vm;
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
        for (self.chunk.constants.items) |constant| {
            if (std.meta.activeTag(constant) == .STRING) self.allocator.free(constant.STRING);
        }
        self.chunk.deinit();

        self.globals.deinit();
        for (self.objects.items) |obj| {
            self.allocator.free(obj.STRING.STRING);
        }
        self.objects.deinit();
    }

    pub fn interpret(self: *VM, alloc: std.mem.Allocator, source: []const u8) InterpretResult {
        self.stack.clearRetainingCapacity();

        if (!compiler.compile(alloc, source, &self.chunk)) {
            self.chunk.deinit();
            return .INTERPRET_COMPILE_ERROR;
        }
        self.ip = self.chunk.code.items;

        const result = self.run() catch |err| {
            std.debug.print("Error: {}\n", .{err});

            return .INTERPRET_RUNTIME_ERROR;
        };

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
                .OP_JUMP_IF_FALSE => {
                    const count = self.readShort();
                    if (isFalsey(self.peek(0).*)) self.ip = self.ip[count..];
                },

                .OP_RETURN => {},
                .OP_PRINT => {
                    try values.printValue(self.stack.pop());
                    try stdout.print("\n", .{});
                },
                .OP_NEGATE => self.negate(),
                .OP_ADD => self.add(),
                .OP_SUBTRACT => self.subtract(),
                .OP_MULTIPLY => self.multiply(),
                .OP_DIVIDE => self.divide(),
                .OP_NOT => self.stack.append(values.Value{ .BOOL = isFalsey(self.stack.pop()) }),
                .OP_FALSE => self.stack.append(values.Value{ .BOOL = false }),
                .OP_POP => _ = self.stack.pop(),
                .OP_GET_LOCAL => {
                    const slot = self.readByte();
                    try self.stack.append(self.stack.items[slot]);
                },
                .OP_SET_LOCAL => {
                    const slot = self.readByte();
                    self.stack.items[slot] = self.peek(0).*;
                },
                .OP_GET_GLOBAL => {
                    const name = self.readString();

                    if (self.globals.get(name.STRING)) |val| {
                        try self.stack.append(val);
                    } else {
                        self.runtimeError("Undefined variable {s}.", .{name.STRING});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .OP_SET_GLOBAL => {
                    const name = self.readString();
                    const val = self.peek(0).*;
                    if (self.globals.contains(name.STRING)) {
                        try self.globals.put(name.STRING, val);
                    } else {
                        self.runtimeError("Undefined variable {s}.", .{name.STRING});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .OP_DEFINE_GLOBAL => {
                    const name = self.readString();
                    const val = self.stack.pop();
                    try self.globals.put(name.STRING, val);
                },
                .OP_EQUAL => self.equal(),
                .OP_GREATER => self.greater(),
                .OP_LESS => self.less(),
                .OP_TRUE => self.stack.append(values.Value{ .BOOL = true }),
                .OP_NIL => self.stack.append(values.Value.NIL),
            };
        }

        return .INTERPRET_OK;
    }

    fn runtimeError(self: *VM, comptime format: []const u8, args: anytype) void {
        // const ArgsType = @TypeOf(args);
        // std.debug.assert(ArgsType == std.builtin.Type.Struct);

        std.debug.print(format, args);

        const line = self.chunk.getLine(self.offset());
        std.debug.print("[line {d} in script]\n", .{line});
        std.debug.print("\n", .{});
    }

    fn equal(self: *VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();

        try switch (a) {
            .STRING => self.stack.append(values.Value{ .BOOL = std.mem.eql(u8, a.STRING, b.STRING) }),
            else => self.stack.append(values.Value{ .BOOL = std.meta.eql(a, b) }),
        };
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
        const val = self.peek(0);
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

        if (std.meta.activeTag(a) == values.ValueTypes.STRING) {
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();
            const allocator = arena.allocator();

            const new_string = values.Value{ .STRING = try std.mem.concat(self.allocator, u8, &.{ a.STRING, values.toString(allocator, b) }) };
            try self.stack.append(new_string);
            try self.objects.append(values.Obj{ .STRING = new_string });

            return;
        }

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

    fn peek(self: *VM, distance: usize) *values.Value {
        return &self.stack.items[(self.stack.items.len - 1) - distance];
    }

    fn readByte(self: *VM) u8 {
        const byte = self.ip[0];
        self.ip = self.ip[1..];
        return byte;
    }

    fn offset(self: *VM) usize {
        return self.chunk.code.items.len - self.ip.len;
    }

    fn readShort(self: *VM) u16 {
        const a: u16 = @as(u16, self.ip[0]) << 8;
        const short_int = a | self.ip[1];
        self.ip = self.ip[2..];
        return short_int;
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

    fn readString(self: *VM) values.Value {
        if (self.chunk.constants.items.len < std.math.maxInt(u8)) {
            return self.readConstant();
        } else {
            return self.readConstantLong();
        }
    }
};

fn isFalsey(val: values.Value) bool {
    return switch (val) {
        .BOOL => !val.BOOL,
        .NIL => true,
        else => false,
    };
}
