const std = @import("std");
const Scanner = @import("scanner.zig");
const values = @import("value.zig");
const util = @import("util.zig");
const debug = @import("debug.zig");
const chunk = @import("chunk.zig");

const Precedence = enum {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR, // or
    PREC_AND, // and
    PREC_EQUALITY, // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM, // + -
    PREC_FACTOR, // * /
    PREC_UNARY, // ! -
    PREC_CALL, // . ()
    PREC_PRIMAR,
};

const ExpressionType = enum {
    GROUPING,
    BINARY,
    UNARY,
    NUMBER,
    LITERAL,
    STRING,
    VARIABLE,
    AND,
    OR,
};

const RULES = [_]ParseRule{
    .{ .prefix = .GROUPING, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_LEFT_PAREN
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_RIGHT_PAREN]
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_LEFT_BRACE]
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_RIGHT_BRACE
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_COMMA
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_DOT
    .{ .prefix = .UNARY, .infix = .BINARY, .precedence = .PREC_TERM }, // .TOKEN_MINUS
    .{ .prefix = undefined, .infix = .BINARY, .precedence = .PREC_TERM }, // .TOKEN_PLUS
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_SEMICOLON
    .{ .prefix = undefined, .infix = .BINARY, .precedence = .PREC_FACTOR }, // .TOKEN_SLASH
    .{ .prefix = undefined, .infix = .BINARY, .precedence = .PREC_FACTOR }, // .TOKEN_STAR
    .{ .prefix = .UNARY, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_BANG
    .{ .prefix = undefined, .infix = .BINARY, .precedence = .PREC_EQUALITY }, // .TOKEN_BANG_EQUAL
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_EQUAL
    .{ .prefix = undefined, .infix = .BINARY, .precedence = .PREC_EQUALITY }, // .TOKEN_EQUAL_EQUAL
    .{ .prefix = undefined, .infix = .BINARY, .precedence = .PREC_COMPARISON }, // .TOKEN_GREATER
    .{ .prefix = undefined, .infix = .BINARY, .precedence = .PREC_COMPARISON }, // .TOKEN_GREATER_EQUAL
    .{ .prefix = undefined, .infix = .BINARY, .precedence = .PREC_COMPARISON }, // .TOKEN_LESS
    .{ .prefix = undefined, .infix = .BINARY, .precedence = .PREC_COMPARISON }, // .TOKEN_LESS_EQUAL
    .{ .prefix = .VARIABLE, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_IDENTIFIER
    .{ .prefix = .STRING, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_STRING
    .{ .prefix = .NUMBER, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_NUMBER
    .{ .prefix = undefined, .infix = .AND, .precedence = .PREC_AND }, // .TOKEN_AND
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_CLASS
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_ELSE
    .{ .prefix = .LITERAL, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_FALSE
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_FOR
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_FUN
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_IF
    .{ .prefix = .LITERAL, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_NIL
    .{ .prefix = undefined, .infix = .OR, .precedence = .PREC_OR }, // .TOKEN_OR
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_PRINT
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_RETURN
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_SUPER
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_THIS
    .{ .prefix = .LITERAL, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_TRUE
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_VAR
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_WHILE
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_ERROR
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE }, // .TOKEN_EOF
};

const ParseRule = struct {
    prefix: ExpressionType,
    infix: ExpressionType,
    precedence: Precedence,
};

fn getRule(t_type: Scanner.TokenType) ParseRule {
    return RULES[@intFromEnum(t_type)];
}

const UINT8_COUNT = std.math.maxInt(u8);

const Compiler = struct {
    parser: Parser,
    function: values.Function,
    function_type: values.FunctionType,
    locals: [UINT8_COUNT]Local,
    local_count: u8,
    scope_depth: u8,
    allocator: std.mem.Allocator,

    fn init(alloctor: std.mem.Allocator, src: []const u8, dst: *chunk.Chunk) Compiler {
        return Compiler{
            .parser = Parser.new(src),
            .function = values.Function.newFrom(dst),
            .function_type = values.FunctionType.TYPE_SCRIPT,
            .locals = undefined,
            .local_count = 0,
            .scope_depth = 0,
            .allocator = alloctor,
        };
    }

    fn and_(self: *Compiler, _: bool) void {
        const end_jump = self.emitJump(@intFromEnum(chunk.opCode.OP_JUMP_IF_FALSE)) catch unreachable;
        self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch logError("error writing byte", self.parser.scanner.line);
        self.parsePrecedence(.PREC_AND);
        self.patchJump(end_jump);
    }

    fn or_(self: *Compiler, _: bool) void {
        const else_jump = self.emitJump(@intFromEnum(chunk.opCode.OP_JUMP_IF_FALSE)) catch unreachable;
        const end_jump = self.emitJump(@intFromEnum(chunk.opCode.OP_JUMP)) catch unreachable;

        self.patchJump(else_jump);
        self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch logError("error writing byte", self.parser.scanner.line);

        self.parsePrecedence(.PREC_OR);
        self.patchJump(end_jump);
    }

    fn emitLoop(self: *Compiler, loop_start: usize) void {
        self.emitByte(@intFromEnum(chunk.opCode.OP_LOOP)) catch unreachable;

        const offset = self.function.byte_code.code.items.len - 1 - loop_start + 2;
        if (offset > std.math.maxInt(u16)) self.parser.reportError("Loop Body too large.");

        self.emitByte(@intCast((offset >> 8) & 0xff)) catch unreachable;
        self.emitByte(@intCast(offset & 0xff)) catch unreachable;
    }
    fn emitJump(self: *Compiler, instruction: u8) !usize {
        try self.emitByte(instruction);
        try self.emitByte(0xff);
        try self.emitByte(0xff);
        return self.function.byte_code.code.items.len - 2;
    }

    fn emitByte(self: *Compiler, byte: u8) !void {
        try self.function.byte_code.writeChunk(byte, self.parser.previous.line);
    }

    fn emitBytes(self: *Compiler, a: u8, b: u8) !void {
        try self.emitByte(a);
        try self.emitByte(b);
    }

    fn emitBytesLong(self: *Compiler, a: u8, b: u24) !void {
        try self.emitByte(a);
        if (b < std.math.maxInt(u8)) {
            try self.function.byte_code.writeChunk(@intCast(b), self.parser.previous.line);
        } else {
            try self.function.byte_code.writeLong(@intCast(b), self.parser.previous.line);
        }
    }
    fn endCompiler(self: *Compiler) !void {
        try self.emitByte(@intFromEnum(chunk.opCode.OP_RETURN));
        if (util.config.debug) {
            if (!self.parser.had_error) {
                debug.disassembleChunk(self.function.byte_code, "code");
                std.debug.print("\n", .{});
            }
        }
    }

    fn emitConstant(self: *Compiler, val: values.Value) !usize {
        const idx = self.makeConstant(val);
        const op = if (idx < std.math.maxInt(u8)) chunk.opCode.OP_CONSTANT else chunk.opCode.OP_CONSTANT_LONG;
        try self.emitByte(@intFromEnum(op));

        if (idx < std.math.maxInt(u8)) {
            try self.function.byte_code.writeChunk(@intCast(idx), self.parser.previous.line);
        } else {
            try self.function.byte_code.writeLong(@intCast(idx), self.parser.previous.line);
        }

        return idx;
    }

    // statements

    fn declaration(self: *Compiler) void {
        if (self.parser.match(.TOKEN_VAR)) {
            self.varDeclaration() catch {
                self.parser.reportError("error declaring variable.");
            };
        } else {
            self.statement();
        }

        if (self.parser.panic_mode) self.parser.synchronize();
    }

    fn addLocal(self: *Compiler, name: Scanner.Token) void {
        if (self.local_count == UINT8_COUNT) {
            self.parser.reportError("Too many local variables in function.");
            return;
        }
        const local = Local{
            .name = name,
            .depth = -1,
        };

        self.locals[self.local_count] = local;
        self.local_count += 1;
    }

    fn parseVariable(self: *Compiler, msg: []const u8) ?u24 {
        self.parser.consume(.TOKEN_IDENTIFIER, msg);

        self.declareVariable();
        if (self.scope_depth > 0) return null;

        return self.identiferConstant(&self.parser.previous);
    }

    fn identiferConstant(self: *Compiler, name: *Scanner.Token) u24 {
        const str = self.allocator.dupe(u8, name.data[0..name.length]) catch unreachable;
        return @intCast(self.makeConstant(values.Value{ .STRING = str }));
    }

    fn makeConstant(self: *Compiler, value: values.Value) usize {
        const idx = self.function.byte_code.addConstant(value) catch unreachable;
        return idx;
    }

    fn declareVariable(self: *Compiler) void {
        if (self.scope_depth == 0) return;

        const name: *Scanner.Token = &self.parser.previous;

        var distance: usize = 1;
        while (distance < self.locals.len) : (distance += 1) {
            const local = self.locals[self.locals.len - distance];
            if (local.depth != -1 and local.depth < self.scope_depth) break;
            if (cmpIdentifier(name.*, local.name)) {
                self.parser.reportError("Already a variable with this name in scope.");
                return;
            }
        }

        self.addLocal(name.*);
    }

    fn defineVariable(self: *Compiler, global: u24) void {
        if (self.scope_depth > 0) {
            markInitialized(self);
            return;
        }
        self.emitBytesLong(@intFromEnum(chunk.opCode.OP_DEFINE_GLOBAL), global) catch unreachable;
    }

    fn varDeclaration(self: *Compiler) !void {
        const global = self.parseVariable("Expect Variable name") orelse 0;

        if (self.parser.match(.TOKEN_EQUAL)) {
            self.expression();
        } else {
            try self.emitByte(@intFromEnum(chunk.opCode.OP_NIL));
        }

        self.parser.consume(.TOKEN_SEMICOLON, "Expect ; after a statement.");
        self.defineVariable(global);
    }

    fn block(self: *Compiler) !void {
        while (self.parser.current.type != .TOKEN_RIGHT_BRACE and self.parser.current.type != .TOKEN_EOF) {
            self.declaration();
        }
        self.parser.consume(.TOKEN_RIGHT_BRACE, "Expect '}' after block.");
    }

    fn forStatement(self: *Compiler) void {
        beginScope(self);
        self.parser.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");

        // initializer
        switch (self.parser.current.type) {
            .TOKEN_SEMICOLON => self.parser.advance(),
            .TOKEN_VAR => {
                self.parser.advance();
                self.varDeclaration() catch unreachable;
            },
            else => {
                self.expression();
                self.parser.consume(.TOKEN_SEMICOLON, "Expect ';' after expression.");
                self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch unreachable;
            },
        }

        var loop_start: usize = self.function.byte_code.code.items.len - 1;
        var exit_jump: isize = -1;

        if (!self.parser.match(.TOKEN_SEMICOLON)) {
            self.expression();
            self.parser.consume(.TOKEN_SEMICOLON, "Expect ';' after loop condition.");

            // jump out of the loop if condition is false
            const jmp = self.emitJump(@intFromEnum(chunk.opCode.OP_JUMP_IF_FALSE)) catch unreachable;
            exit_jump = @intCast(jmp);
            self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch unreachable;
        }

        if (!self.parser.match(.TOKEN_RIGHT_PAREN)) {
            const body_jump = self.emitJump(@intFromEnum(chunk.opCode.OP_JUMP)) catch unreachable;
            const increment_start = self.function.byte_code.code.items.len - 1;
            self.expression();
            self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch unreachable;
            std.debug.print("{any}\n", .{self.parser.current.type});
            self.parser.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

            self.emitLoop(loop_start);
            loop_start = increment_start;
            self.patchJump(body_jump);
        }

        self.statement();
        self.emitLoop(loop_start);

        if (exit_jump != -1) {
            self.patchJump(@intCast(exit_jump));
            self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch unreachable;
        }

        endScope(self);
    }

    fn ifStatement(self: *Compiler) void {
        self.parser.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
        self.expression();
        self.parser.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after 'if'.");

        const then_jump = self.emitJump(@intFromEnum(chunk.opCode.OP_JUMP_IF_FALSE)) catch unreachable;
        self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch unreachable;
        self.statement();

        const else_jump = self.emitJump(@intFromEnum(chunk.opCode.OP_JUMP)) catch unreachable;
        self.patchJump(then_jump);
        self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch unreachable;

        if (self.parser.match(.TOKEN_ELSE)) self.statement();
        self.patchJump(else_jump);
    }

    fn whileStatement(self: *Compiler) void {
        const loop_start = self.function.byte_code.code.items.len - 1;

        self.parser.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
        self.expression();
        self.parser.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after 'if'.");

        const exit_jump = self.emitJump(@intFromEnum(chunk.opCode.OP_JUMP_IF_FALSE)) catch unreachable;
        self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch unreachable;
        self.statement();
        self.emitLoop(loop_start);

        self.patchJump(exit_jump);
        self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch unreachable;
    }

    fn patchJump(self: *Compiler, offset: usize) void {
        const jump = self.function.byte_code.code.items.len - offset - 2;

        if (jump > std.math.maxInt(u16)) self.parser.reportError("Too much code to jump over");

        std.debug.assert(self.function.byte_code.code.items[offset] == 0xff);
        std.debug.assert(self.function.byte_code.code.items[offset + 1] == 0xff);
        self.function.byte_code.code.items[offset] = @intCast((jump >> 8) & 0xff);
        self.function.byte_code.code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn statement(self: *Compiler) void {
        if (self.parser.match(.TOKEN_PRINT)) {
            self.expression();
            self.parser.consume(.TOKEN_SEMICOLON, "Expect ; after a.");
            self.emitByte(@intFromEnum(chunk.opCode.OP_PRINT)) catch |err| {
                std.debug.print("Error writing op: {}\n", .{err});
            };
        } else if (self.parser.match(.TOKEN_FOR)) {
            self.forStatement();
        } else if (self.parser.match(.TOKEN_IF)) {
            self.ifStatement();
        } else if (self.parser.match(.TOKEN_WHILE)) {
            self.whileStatement();
        } else if (self.parser.match(.TOKEN_LEFT_BRACE)) {
            beginScope(self);
            self.block() catch unreachable;
            endScope(self);
        } else {
            self.expression();
            self.parser.consume(.TOKEN_SEMICOLON, "Expect ; after a statement.");
            self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch unreachable;
        }
    }

    // expressions

    fn expression(self: *Compiler) void {
        self.parsePrecedence(.PREC_ASSIGNMENT);
    }

    // Parse number literals
    fn number(self: *Compiler) !void {
        const val = try std.fmt.parseFloat(f64, self.parser.previous.data[0..self.parser.previous.length]);
        _ = try self.emitConstant(values.Value{ .NUMBER = val });
    }

    // Parse grouping expressions
    fn grouping(self: *Compiler) void {
        self.expression();
        self.parser.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }

    // Parse unary expression
    fn unary(self: *Compiler) void {
        const op_type = self.parser.previous.type;

        self.parsePrecedence(.PREC_UNARY);

        _ = switch (op_type) {
            .TOKEN_MINUS => self.emitByte(@intFromEnum(chunk.opCode.OP_NEGATE)),
            .TOKEN_BANG => self.emitByte(@intFromEnum(chunk.opCode.OP_NOT)),
            else => {},
        } catch unreachable;
    }

    // Parse binary expression
    fn binary(self: *Compiler) void {
        const op_type = self.parser.previous.type;
        // parse rule?
        const precedence = getRule(op_type).precedence;
        self.parsePrecedence(@enumFromInt(@intFromEnum(precedence) + 1));

        _ = switch (op_type) {
            .TOKEN_BANG_EQUAL => self.emitBytes(@intFromEnum(chunk.opCode.OP_EQUAL), @intFromEnum(chunk.opCode.OP_NOT)),
            .TOKEN_EQUAL_EQUAL => self.emitByte(@intFromEnum(chunk.opCode.OP_EQUAL)),
            .TOKEN_GREATER => self.emitByte(@intFromEnum(chunk.opCode.OP_GREATER)),
            .TOKEN_GREATER_EQUAL => self.emitBytes(@intFromEnum(chunk.opCode.OP_LESS), @intFromEnum(chunk.opCode.OP_NOT)),
            .TOKEN_LESS => self.emitByte(@intFromEnum(chunk.opCode.OP_LESS)),
            .TOKEN_LESS_EQUAL => self.emitBytes(@intFromEnum(chunk.opCode.OP_GREATER), @intFromEnum(chunk.opCode.OP_NOT)),
            .TOKEN_PLUS => self.emitByte(@intFromEnum(chunk.opCode.OP_ADD)),
            .TOKEN_MINUS => self.emitByte(@intFromEnum(chunk.opCode.OP_SUBTRACT)),
            .TOKEN_STAR => self.emitByte(@intFromEnum(chunk.opCode.OP_MULTIPLY)),
            .TOKEN_SLASH => self.emitByte(@intFromEnum(chunk.opCode.OP_DIVIDE)),
            else => {},
        } catch |err| {
            std.debug.print("{} writing operator {}\n", .{ err, op_type });
        };
    }

    fn literal(self: *Compiler) void {
        _ = switch (self.parser.previous.type) {
            .TOKEN_FALSE => self.emitByte(@intFromEnum(chunk.opCode.OP_FALSE)),
            .TOKEN_NIL => self.emitByte(@intFromEnum(chunk.opCode.OP_NIL)),
            .TOKEN_TRUE => self.emitByte(@intFromEnum(chunk.opCode.OP_TRUE)),
            else => {},
        } catch |err| {
            std.debug.print("{} writing literal {}\n", .{ err, self.parser.previous.type });
        };
    }

    fn string(self: *Compiler) void {
        const str = self.allocator.dupe(u8, self.parser.previous.data[0..self.parser.previous.length]) catch |err| {
            std.debug.print("Error copying string: {}\n", .{err});
            return;
        };

        _ = self.emitConstant(values.Value{ .STRING = str }) catch |err| {
            std.debug.print("Error writing string: {}\n", .{err});
            return;
        };
    }

    fn variable(self: *Compiler, can_assign: bool) void {
        self.namedVariable(self.parser.previous, can_assign);
    }

    fn namedVariable(self: *Compiler, name: Scanner.Token, can_assign: bool) void {
        var get_op: u8 = 0;
        var set_op: u8 = 0;
        var arg = self.resolveLocal(self, name);

        if (arg != -1) {
            get_op = @intFromEnum(chunk.opCode.OP_GET_LOCAL);
            set_op = @intFromEnum(chunk.opCode.OP_SET_LOCAL);
        } else {
            get_op = @intFromEnum(chunk.opCode.OP_GET_GLOBAL);
            set_op = @intFromEnum(chunk.opCode.OP_SET_GLOBAL);
            var id = name;
            arg = self.identiferConstant(&id);
        }

        if (can_assign and self.parser.match(.TOKEN_EQUAL)) {
            self.expression();

            self.emitBytes(set_op, @intCast(arg)) catch unreachable;
        } else {
            self.emitBytes(get_op, @intCast(arg)) catch unreachable;
        }
    }

    fn eval(self: *Compiler, expr_type: ExpressionType, can_assign: bool) void {
        switch (expr_type) {
            .GROUPING => self.grouping(),
            .BINARY => self.binary(),
            .UNARY => self.unary(),
            .NUMBER => self.number() catch unreachable,
            .LITERAL => self.literal(),
            .STRING => self.string(),
            .VARIABLE => self.variable(can_assign),
            .AND => self.and_(can_assign),
            .OR => self.or_(can_assign),
        }
    }

    fn parsePrecedence(self: *Compiler, precedence: Precedence) void {
        self.parser.advance();
        const rule = getRule(self.parser.previous.type);

        if (rule.prefix == undefined) {
            self.parser.reportError("Expect expression.");
            return;
        }

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.PREC_ASSIGNMENT);
        self.eval(rule.prefix, can_assign);
        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.parser.current.type).precedence)) {
            self.parser.advance();
            const infix_rule = getRule(self.parser.previous.type).infix;
            self.eval(infix_rule, can_assign);
        }
    }

    fn resolveLocal(self: *Compiler, compiler: *Compiler, name: Scanner.Token) isize {
        if (compiler.local_count == 0) return -1;
        var start: isize = compiler.local_count - 1;

        while (start >= 0) : (start -= 1) {
            const local = compiler.locals[@intCast(start)];
            if (cmpIdentifier(name, local.name)) {
                if (local.depth == -1) self.parser.reportError("Can't read local variable in its own initializer");
                return @intCast(start);
            }
        }

        return -1;
    }
};

const Local = struct {
    name: Scanner.Token,
    depth: i8,
};

// const ParseFn = fn () void;

const Parser = struct {
    current: Scanner.Token,
    previous: Scanner.Token,
    had_error: bool,
    panic_mode: bool,
    scanner: Scanner,

    fn new(src: []const u8) Parser {
        return Parser{
            .current = undefined,
            .previous = undefined,
            .had_error = false,
            .panic_mode = false,
            .scanner = Scanner.new(src),
        };
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;

        while (self.current.type != .TOKEN_EOF) {
            const token = self.scanner.scanToken();
            self.previous = self.current;
            self.current = token;
            if (self.current.type != .TOKEN_ERROR) break;
            self.errorAt(&self.current, &self.current.data);
        }
    }

    fn consume(self: *Parser, token_type: Scanner.TokenType, msg: []const u8) void {
        if (self.current.type == token_type) {
            self.advance();
            return;
        }
        self.errorAt(&self.current, msg);
    }

    fn match(self: *Parser, token_type: Scanner.TokenType) bool {
        if (self.current.type != token_type) return false;
        self.advance();
        return true;
    }

    // fn and_(self: *Parser, _: bool) void {
    //     const end_jump = self.emitJump(@intFromEnum(chunk.opCode.OP_JUMP_IF_FALSE)) catch unreachable;
    //     self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch logError("error writing byte", self.scanner.line);
    //     self.parsePrecedence(.PREC_AND);
    //     self.patchJump(end_jump);
    // }

    // fn or_(self: *Parser, _: bool) void {
    //     const else_jump = self.emitJump(@intFromEnum(chunk.opCode.OP_JUMP_IF_FALSE)) catch unreachable;
    //     const end_jump = self.emitJump(@intFromEnum(chunk.opCode.OP_JUMP)) catch unreachable;

    //     self.patchJump(else_jump);
    //     self.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch logError("error writing byte", self.scanner.line);

    //     self.parsePrecedence(.PREC_OR);
    //     self.patchJump(end_jump);
    // }

    // fn emitLoop(self: *Parser, loop_start: usize) void {
    //     self.emitByte(@intFromEnum(chunk.opCode.OP_LOOP)) catch unreachable;

    //     const offset = self.chunk.code.items.len - 1 - loop_start + 2;
    //     if (offset > std.math.maxInt(u16)) self.reportError("Loop Body too large.");

    //     self.emitByte(@intCast((offset >> 8) & 0xff)) catch unreachable;
    //     self.emitByte(@intCast(offset & 0xff)) catch unreachable;
    // }
    // fn emitJump(self: *Parser, instruction: u8) !usize {
    //     try self.emitByte(instruction);
    //     try self.emitByte(0xff);
    //     try self.emitByte(0xff);
    //     return self.chunk.code.items.len - 2;
    // }

    // fn emitByte(self: *Parser, byte: u8) !void {
    //     try self.chunk.writeChunk(byte, self.previous.line);
    // }

    // fn emitBytes(self: *Parser, a: u8, b: u8) !void {
    //     try self.emitByte(a);
    //     try self.emitByte(b);
    // }

    // fn emitBytesLong(self: *Parser, a: u8, b: u24) !void {
    //     try self.emitByte(a);
    //     if (b < std.math.maxInt(u8)) {
    //         try self.chunk.writeChunk(@intCast(b), self.previous.line);
    //     } else {
    //         try self.chunk.writeLong(@intCast(b), self.previous.line);
    //     }
    // }

    fn reportError(self: *Parser, msg: []const u8) void {
        self.errorAt(&self.previous, msg);
    }

    fn errorAt(self: *Parser, token: *Scanner.Token, message: []const u8) void {
        if (self.panic_mode) return;
        self.panic_mode = true;
        std.debug.print("[line {d}] Error", .{token.line});

        switch (token.type) {
            .TOKEN_EOF => std.debug.print(" at end", .{}),
            .TOKEN_ERROR => {},
            else => std.debug.print(" at {s}", .{token.data[0..token.length]}),
        }

        std.debug.print(": {s\n}", .{message});
        self.had_error = true;
    }

    fn synchronize(self: *Parser) void {
        self.panic_mode = false;

        while (self.current.type != .TOKEN_EOF) {
            if (self.previous.type == .TOKEN_SEMICOLON) break;
            switch (self.current.type) {
                .TOKEN_CLASS,
                .TOKEN_FUN,
                .TOKEN_VAR,
                .TOKEN_FOR,
                .TOKEN_IF,
                .TOKEN_WHILE,
                .TOKEN_PRINT,
                .TOKEN_RETURN,
                => break,
                else => {},
            }

            self.advance();
        }
    }
};

fn cmpIdentifier(a: Scanner.Token, b: Scanner.Token) bool {
    if (a.length != b.length) return false;
    return std.mem.eql(u8, a.data[0..a.length], b.data[0..b.length]);
}

fn markInitialized(compiler: *Compiler) void {
    if (compiler.scope_depth == 0) return;
    compiler.locals[compiler.local_count - 1].depth = @intCast(compiler.scope_depth);
}

fn beginScope(compiler: *Compiler) void {
    compiler.scope_depth += 1;
}

fn endScope(compiler: *Compiler) void {
    compiler.scope_depth -= 1;

    while (compiler.local_count > 0 and compiler.locals[compiler.local_count - 1].depth > compiler.scope_depth) {
        compiler.emitByte(@intFromEnum(chunk.opCode.OP_POP)) catch unreachable;
        compiler.local_count -= 1;
    }
}

fn logError(msg: []const u8, line: usize) void {
    std.log.err("{s} line {d}\n", .{ msg, line });
}

pub fn compile(gpa: std.mem.Allocator, src: []const u8, dst: *chunk.Chunk) bool {
    var compiler = Compiler.init(gpa, src, dst);

    compiler.parser.advance();

    while (!compiler.parser.match(.TOKEN_EOF)) {
        compiler.declaration();
    }

    compiler.endCompiler() catch |err| {
        std.debug.print("{} when ending compilation", .{err});
    };

    return !compiler.parser.had_error;
}
