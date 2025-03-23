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
};

const RULES = [_]ParseRule{
    .{ .prefix = .GROUPING, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = .UNARY, .infix = .BINARY, .precedence = .PREC_TERM },
    .{ .prefix = undefined, .infix = .BINARY, .precedence = .PREC_TERM },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = .BINARY, .precedence = .PREC_FACTOR },
    .{ .prefix = undefined, .infix = .BINARY, .precedence = .PREC_FACTOR },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = .NUMBER, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
    .{ .prefix = undefined, .infix = undefined, .precedence = .PREC_NONE },
};

const ParseRule = struct {
    prefix: ExpressionType,
    infix: ExpressionType,
    precedence: Precedence,
};

fn getRule(t_type: Scanner.TokenType) ParseRule {
    return RULES[@intFromEnum(t_type)];
}

const ParseFn = fn () void;

const Parser = struct {
    current: Scanner.Token,
    previous: Scanner.Token,
    had_error: bool,
    panic_mode: bool,
    scanner: *Scanner,
    chunk: *chunk.Chunk,

    fn new(scanner: *Scanner, chk: *chunk.Chunk) Parser {
        return Parser{
            .current = undefined,
            .previous = undefined,
            .had_error = false,
            .panic_mode = false,
            .scanner = scanner,
            .chunk = chk,
        };
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;

        while (self.scanner.scanToken()) |token| {
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

    fn emitByte(self: *Parser, byte: u8) !void {
        try self.chunk.writeChunk(byte, self.previous.line);
    }

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

    fn endCompiler(self: *Parser) !void {
        try self.emitByte(@intFromEnum(chunk.opCode.OP_RETURN));
        if (util.config.debug) {
            if (!self.had_error) {
                debug.disassembleChunk(self.chunk, "code");
            }
        }
    }

    fn emitBytes(self: *Parser, a: u8, b: u8) void {
        try self.emitByte(a);
        try self.emitByte(b);
    }

    fn emitConstant(self: *Parser, val: values.Value) !void {
        try self.chunk.writeConstant(val, self.previous.line);
    }

    //expressions

    fn expression(self: *Parser) void {
        self.parsePrecedence(.PREC_ASSIGNMENT);
    }

    // Parse number literals
    fn number(self: *Parser) !void {
        const val = try std.fmt.parseFloat(f64, self.previous.data[0..self.previous.length]);
        try self.emitConstant(val);
    }

    // Parse grouping expressions
    fn grouping(self: *Parser) void {
        self.expression();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }

    // Parse unary expression
    fn unary(self: *Parser) void {
        const op_type = self.previous.type;

        self.parsePrecedence(.PREC_UNARY);

        _ = switch (op_type) {
            .TOKEN_MINUS => self.emitByte(@intFromEnum(chunk.opCode.OP_NEGATE)),
            else => {},
        } catch unreachable;
    }

    // Parse binary expression
    fn binary(self: *Parser) void {
        const op_type = self.previous.type;
        // parse rule?
        const precedence = getRule(op_type).precedence;

        self.parsePrecedence(@enumFromInt(@intFromEnum(precedence) + 1));

        _ = switch (op_type) {
            .TOKEN_PLUS => self.emitByte(@intFromEnum(chunk.opCode.OP_ADD)),
            .TOKEN_MINUS => self.emitByte(@intFromEnum(chunk.opCode.OP_SUBTRACT)),
            .TOKEN_STAR => self.emitByte(@intFromEnum(chunk.opCode.OP_MULTIPLY)),
            .TOKEN_SLASH => self.emitByte(@intFromEnum(chunk.opCode.OP_DIVIDE)),
            else => {},
        } catch |err| {
            std.debug.print("{} writing operator {}", .{ err, op_type });
        };
    }

    fn eval(self: *Parser, expr_type: ExpressionType) void {
        switch (expr_type) {
            .GROUPING => self.grouping(),
            .BINARY => self.binary(),
            .UNARY => self.unary(),
            .NUMBER => self.number() catch unreachable,
        }
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) void {
        self.advance();
        const rule = getRule(self.previous.type);

        if (rule.prefix == undefined) {
            self.reportError("Expect expression.");
            return;
        }
        self.eval(rule.prefix);
        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.type).precedence)) {
            self.advance();
            const infix_rule = getRule(self.previous.type).infix;
            self.eval(infix_rule);
        }
    }
};

pub fn compile(src: []const u8, dst: *chunk.Chunk) bool {
    var scanner = Scanner.new(src);
    var parser = Parser.new(&scanner, dst);
    parser.advance();
    parser.expression();
    parser.endCompiler() catch |err| {
        std.debug.print("{} when ending compilation", .{err});
    };

    return !parser.had_error;
}
