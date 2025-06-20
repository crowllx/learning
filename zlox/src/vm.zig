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
const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * std.math.maxInt(u8);

const CallFrame = struct {
    closure: values.Closure,
    ip: []u8,
    slots: []values.Value,
    slot_start: usize,
};

pub const VM = struct {
    frames: [FRAMES_MAX]CallFrame,
    frame_count: usize,
    stack: [STACK_MAX]values.Value,
    stack_top: usize,
    globals: std.StringHashMap(values.Value),
    objects: std.ArrayList(values.Obj),
    open_upvalues: ?*values.Upvalue,
    allocator: std.mem.Allocator,

    pub fn init(gpa: std.mem.Allocator) !VM {
        var vm = VM{
            .frames = undefined,
            .frame_count = 0,
            .stack = undefined,
            .stack_top = 0,
            .objects = std.ArrayList(values.Obj).init(gpa),
            .globals = std.StringHashMap(values.Value).init(gpa),
            .open_upvalues = null,
            .allocator = gpa,
        };
        vm.defineNative("clock", @ptrCast(&clockNative));
        return vm;
    }

    pub fn deinit(self: *VM) void {
        self.globals.deinit();
        self.frames[0].closure.function.deinit();
        for (self.objects.items) |obj| {
            self.allocator.free(obj.STRING);
        }
        self.objects.deinit();
    }

    pub fn interpret(self: *VM, alloc: std.mem.Allocator, source: []const u8) InterpretResult {
        var function: values.Function = undefined;
        if (compiler.compile(alloc, source)) |func| {
            function = func;
        } else return .INTERPRET_COMPILE_ERROR;

        self.push(values.Value{ .OBJ = .{ .FUNCTION = function } });
        const closure = values.newClosure(self.allocator, function) catch {
            self.runtimeError("error creating closure\n", .{});
            return .INTERPRET_RUNTIME_ERROR;
        };
        _ = self.pop();
        self.push(values.Value{ .OBJ = .{ .CLOSURE = closure } });
        _ = self.call(closure, 0);

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
        while (self.currentFrame().ip.len > 0) {
            const off = self.offset();
            const next = self.readByte();

            const instruction: chunk.opCode = std.meta.intToEnum(chunk.opCode, next) catch |err| {
                std.debug.print("Error: {} offset: {} instruction: {}\n", .{ err, self.offset(), next });
                return .INTERPRET_COMPILE_ERROR;
            };

            if (util.config.debug) {
                _ = debug.disassembleInstruction(off, next, &self.currentFrame().closure.function.byte_code);
                for (self.stack) |v| {
                    switch (v) {
                        .NUMBER => std.debug.print("                [ {d:.2} ]\n", .{v.NUMBER}),
                        else => {},
                    }
                }
            }

            try switch (instruction) {
                .OP_CONSTANT => {
                    const val = self.readConstant();
                    self.push(val);
                },
                .OP_CONSTANT_LONG => {
                    const val = self.readConstantLong();
                    self.push(val);
                },
                .OP_JUMP => {
                    var frame = self.currentFrame();
                    const jump_offset = self.readShort();
                    frame.ip = frame.ip[jump_offset..];
                },
                .OP_JUMP_IF_FALSE => {
                    var frame = self.currentFrame();
                    const jump_offset = self.readShort();
                    if (isFalsey(self.peek(0).*)) frame.ip = frame.ip[jump_offset..];
                },
                .OP_LOOP => {
                    var frame = self.currentFrame();
                    const loop_offset = self.readShort();
                    frame.ip = frame.closure.function.byte_code.code.items[self.offset() - loop_offset ..];
                },
                .OP_CALL => {
                    const arg_count = self.readByte();

                    if (!self.callValue(self.peek(arg_count).*, arg_count)) {
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .OP_CLOSURE => {
                    const function = self.readConstant();
                    const closure = values.newClosure(self.allocator, function.OBJ.FUNCTION) catch {
                        self.runtimeError("Error creating closure.\n", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    };
                    self.push(values.Value{ .OBJ = .{ .CLOSURE = closure } });

                    for (0..closure.upvalue_count) |i| {
                        const is_local = self.readByte();
                        const index = self.readByte();

                        if (is_local == 1) {
                            closure.upvalues.items[i] = self.captureUpvalue(&self.currentFrame().slots[index]);
                        } else {
                            closure.upvalues.items[i] = self.currentFrame().closure.upvalues.items[index];
                        }
                    }
                },
                .OP_CLOSE_UPVALUE => {
                    self.closeUpvalues(&self.stack[self.stack_top - 1]);
                    _ = self.pop();
                },
                .OP_RETURN => {
                    if (self.stack.len == 0) {
                        self.runtimeError("Missing return value.", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    }

                    const result = self.pop();
                    const stack_ptr = self.currentFrame().slot_start;
                    self.closeUpvalues(&self.currentFrame().slots[0]);
                    self.frame_count -= 1;

                    if (self.frame_count == 0) {
                        return .INTERPRET_OK;
                    }
                    self.stack_top = stack_ptr - 1;
                    self.push(result);
                },
                .OP_PRINT => {
                    try values.printValue(self.pop());
                    try stdout.print("\n", .{});
                },
                .OP_NEGATE => self.negate(),
                .OP_ADD => self.add(),
                .OP_SUBTRACT => self.subtract(),
                .OP_MULTIPLY => self.multiply(),
                .OP_DIVIDE => self.divide(),
                .OP_NOT => self.push(values.Value{ .BOOL = isFalsey(self.pop()) }),
                .OP_FALSE => self.push(values.Value{ .BOOL = false }),
                .OP_POP => _ = self.pop(),
                .OP_GET_LOCAL => {
                    const slot = self.readByte();
                    self.push(self.currentFrame().slots[slot - 1]);
                },

                // unsure why index to set/get locals are off by one.
                // started occuring after reserving first slot on stack while compiling
                .OP_SET_LOCAL => {
                    const slot = self.readByte();
                    self.currentFrame().slots[slot - 1] = self.peek(0).*;
                },
                .OP_GET_GLOBAL => {
                    const name = self.readString();

                    if (self.globals.get(name.OBJ.STRING)) |val| {
                        self.push(val);
                    } else {
                        self.runtimeError("Undefined variable {s}.?", .{name.OBJ.STRING});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .OP_SET_GLOBAL => {
                    const name = self.readString();
                    const val = self.peek(0).*;
                    if (self.globals.contains(name.OBJ.STRING)) {
                        try self.globals.put(name.OBJ.STRING, val);
                    } else {
                        self.runtimeError("Undefined variable {s}.", .{name.OBJ.STRING});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .OP_DEFINE_GLOBAL => {
                    const name = self.readString().OBJ.STRING;
                    const val = self.pop();
                    try self.globals.put(name, val);
                },
                .OP_GET_UPVALUE => {
                    const slot = self.readByte();
                    self.push(self.currentFrame().closure.upvalues.items[slot].location.*);
                },
                .OP_SET_UPVALUE => {
                    const slot = self.readByte();
                    self.currentFrame().closure.upvalues.items[slot].location = self.peek(0);
                },
                .OP_EQUAL => self.equal(),
                .OP_GREATER => self.greater(),
                .OP_LESS => self.less(),
                .OP_TRUE => self.push(values.Value{ .BOOL = true }),
                .OP_NIL => self.push(values.Value.NIL),
            };
        }

        return .INTERPRET_OK;
    }

    fn runtimeError(self: *VM, comptime format: []const u8, args: anytype) void {
        // const ArgsType = @TypeOf(args);
        // std.debug.assert(ArgsType == std.builtin.Type.Struct);

        std.debug.print(format, args);

        var i: isize = @intCast(self.frame_count - 1);
        while (i >= 0) : (i -= 1) {
            const frame = &self.frames[@intCast(i)];
            var function = frame.closure.function;
            const instruction = self.offset();
            std.debug.print("[line {d}] in ", .{function.byte_code.getLine(instruction)});
            if (function.name.len > 0) {
                std.debug.print("script\n", .{});
            } else {
                std.debug.print("{s}()\n", .{function.name});
            }
        }
    }

    fn equal(self: *VM) !void {
        const b = self.pop();
        const a = self.pop();

        switch (a) {
            .OBJ => {
                switch (a.OBJ) {
                    .STRING => self.push(values.Value{ .BOOL = std.mem.eql(u8, a.OBJ.STRING, b.OBJ.STRING) }),
                    else => {},
                }
            },
            else => self.push(values.Value{ .BOOL = std.meta.eql(a, b) }),
        }
    }

    fn greater(self: *VM) !void {
        const b = self.pop();
        const a = self.pop();

        std.debug.assert(@as(values.ValueTypes, a) == .NUMBER);
        std.debug.assert(@as(values.ValueTypes, b) == .NUMBER);

        self.push(values.Value{ .BOOL = a.NUMBER > b.NUMBER });
    }

    fn less(self: *VM) !void {
        const b = self.pop();
        const a = self.pop();

        std.debug.assert(@as(values.ValueTypes, a) == .NUMBER);
        std.debug.assert(@as(values.ValueTypes, b) == .NUMBER);

        self.push(values.Value{ .BOOL = a.NUMBER < b.NUMBER });
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
        const b = self.pop();
        const a = self.pop();

        if (std.meta.activeTag(a) == values.ValueTypes.OBJ and std.meta.activeTag(a.OBJ) == values.ObjTypes.STRING) {
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();
            const allocator = arena.allocator();

            const new_string = try std.mem.concat(self.allocator, u8, &.{ a.OBJ.STRING, values.toString(allocator, b) });
            const new_val = values.newString(new_string);
            self.push(new_val);
            try self.objects.append(values.Obj{ .STRING = new_string });

            return;
        }

        std.debug.assert(@as(values.ValueTypes, a) == .NUMBER);
        std.debug.assert(@as(values.ValueTypes, b) == .NUMBER);

        self.push(values.Value{ .NUMBER = a.NUMBER + b.NUMBER });
    }

    fn subtract(self: *VM) !void {
        const b = self.pop();
        const a = self.pop();

        std.debug.assert(@as(values.ValueTypes, a) == .NUMBER);
        std.debug.assert(@as(values.ValueTypes, b) == .NUMBER);

        self.push(values.Value{ .NUMBER = a.NUMBER - b.NUMBER });
    }

    fn multiply(self: *VM) !void {
        const b = self.pop();
        const a = self.pop();

        std.debug.assert(@as(values.ValueTypes, a) == .NUMBER);
        std.debug.assert(@as(values.ValueTypes, b) == .NUMBER);

        self.push(values.Value{ .NUMBER = a.NUMBER * b.NUMBER });
    }

    fn divide(self: *VM) !void {
        const b = self.pop();
        const a = self.pop();

        std.debug.assert(@as(values.ValueTypes, a) == .NUMBER);
        std.debug.assert(@as(values.ValueTypes, b) == .NUMBER);

        self.push(values.Value{ .NUMBER = a.NUMBER / b.NUMBER });
    }

    fn peek(self: *VM, distance: usize) *values.Value {
        return &self.stack[self.stack_top - 1 - distance];
    }

    fn readByte(self: *VM) u8 {
        const frame = self.currentFrame();
        const byte = frame.ip[0];
        frame.ip = frame.ip[1..];
        return byte;
    }

    fn offset(self: *VM) usize {
        const frame = self.currentFrame();
        return frame.closure.function.byte_code.code.items.len - frame.ip.len;
    }

    fn currentFrame(self: *VM) *CallFrame {
        return &self.frames[self.frame_count - 1];
    }

    fn readShort(self: *VM) u16 {
        const frame = self.currentFrame();
        const a: u16 = @as(u16, frame.ip[0]) << 8;
        const short_int = a | frame.ip[1];
        frame.ip = frame.ip[2..];
        return short_int;
    }

    fn readConstant(self: *VM) values.Value {
        const idx = self.readByte();
        return self.currentFrame().closure.function.byte_code.constants.items[idx];
    }

    fn getConstant(self: *VM, idx: usize) values.Value {
        return self.currentFrame().closure.function.byte_code.constants.items[idx];
    }

    fn readConstantLong(self: *VM) values.Value {
        const buf = [3]u8{ self.readByte(), self.readByte(), self.readByte() };
        const idx = util.numFromBytes(buf);
        const val = self.getConstant(idx);
        return val;
    }

    fn readString(self: *VM) values.Value {
        if (self.currentFrame().closure.function.byte_code.constants.items.len < std.math.maxInt(u8)) {
            return self.readConstant();
        } else {
            return self.readConstantLong();
        }
    }

    fn captureUpvalue(self: *VM, local: *values.Value) *values.Upvalue {
        var prev_upvalue: ?*values.Upvalue = null;
        var upvalue = self.open_upvalues;

        while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.next;
        }

        if (upvalue != null and upvalue.?.location == local) {
            return upvalue.?;
        }

        var created_upvalue = values.newUpvalue(local);
        created_upvalue.next = upvalue;

        if (prev_upvalue == null) {
            self.open_upvalues = &created_upvalue;
        } else {
            prev_upvalue.?.next = &created_upvalue;
        }
        return &created_upvalue;
    }

    fn closeUpvalues(self: *VM, last: *values.Value) void {
        while (self.open_upvalues != null and @intFromPtr(self.open_upvalues.?.location) >= @intFromPtr(last)) {
            const upvalue = self.open_upvalues;
            upvalue.?.closed = upvalue.?.location.*;
            upvalue.?.location = &upvalue.?.closed;
            self.open_upvalues = upvalue.?.next;
        }
    }

    fn call(self: *VM, closure: values.Closure, arg_count: u8) bool {
        if (arg_count != closure.function.arity) {
            self.runtimeError("Expected {d} arguments but got {d}.\n", .{ closure.function.arity, arg_count });
            return false;
        }

        if (self.frame_count == FRAMES_MAX) {
            self.runtimeError("Stack overflow.\n", .{});
            return false;
        }

        var frame = &self.frames[self.frame_count];
        self.frame_count += 1;

        frame.closure = closure;
        frame.ip = closure.function.byte_code.code.items;
        frame.slots = self.stack[self.stack_top - arg_count ..];
        frame.slot_start = self.stack_top - arg_count;

        return true;
    }

    fn native(self: *VM, arg_count: u8, nat_fn: values.NativeFn) values.Value {
        var buf = std.ArrayList(values.Value).init(self.allocator);
        defer buf.deinit();

        const func: *const fn (u8, []values.Value) values.Value = @ptrCast(nat_fn);

        var i = arg_count;
        while (i > 0) : (i -= 1) {
            buf.append(self.pop()) catch unreachable;
        }

        const result = func(arg_count, buf.items);

        return result;
    }

    fn defineNative(self: *VM, name: []const u8, nat_fn: values.NativeFn) void {
        const val = values.Value{ .OBJ = values.Obj{ .NATIVE_FUNCTION = nat_fn } };

        const str = self.allocator.dupe(u8, name) catch unreachable;
        self.globals.put(str, val) catch |err| {
            std.debug.print("Error writing to globals. {any}\n", .{err});
        };
    }

    fn callValue(self: *VM, callee: values.Value, arg_count: u8) bool {
        if (std.meta.activeTag(callee) == values.ValueTypes.OBJ) {
            const obj = callee.OBJ;
            switch (obj) {
                .CLOSURE => return self.call(obj.CLOSURE, arg_count),
                .NATIVE_FUNCTION => {
                    const nat_fn = obj.NATIVE_FUNCTION;
                    const result = self.native(arg_count, nat_fn);
                    self.push(result);
                    return true;
                },
                else => {},
            }
        }
        self.runtimeError("Can only call functions and classes.", .{});
        return false;
    }

    fn pop(self: *VM) values.Value {
        self.stack_top -= 1;
        const v = self.stack[self.stack_top];
        return v;
    }

    fn push(self: *VM, v: values.Value) void {
        self.stack[self.stack_top] = v;
        self.stack_top += 1;
    }
};
// Native functions
fn clockNative(arg_count: u8, args: []values.Value) values.Value {
    _ = arg_count;
    _ = args;

    return values.Value{ .NUMBER = @floatFromInt(std.time.timestamp()) };
}

fn isFalsey(val: values.Value) bool {
    return switch (val) {
        .BOOL => !val.BOOL,
        .NIL => true,
        else => false,
    };
}
