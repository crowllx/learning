const std = @import("std");

const Scanner = @This();
// Scanner fields
source: []const u8,
cursor: u8,
line: u16,

pub const TokenType = enum {
    // Single-character tokens.
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,
    // One or two character tokens.
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,
    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,
    // Keywords.
    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,

    TOKEN_ERROR,
    TOKEN_EOF,
};

pub const Token = struct {
    type: TokenType,
    data: [256]u8,
    length: u8,
    line: u16,

    pub fn print(self: *Token) void {
        std.debug.print("{s:<20} line: {d:<3} lexeme: {s}\n", .{ @tagName(self.type), self.line, self.data });
    }
};

pub fn new(source: []const u8) Scanner {
    return Scanner{
        .source = source,
        .cursor = 0,
        .line = 1,
    };
}

pub fn scanToken(self: *Scanner) Token {
    if (self.isAtEnd()) return self.makeToken(.TOKEN_EOF);

    self.skipWhitespace();
    self.skipComment();

    const c = self.advance();
    if (std.ascii.isAlphabetic(c)) return self.makeIdentifier();
    if (std.ascii.isDigit(c)) return self.makeNumber();
    const token = switch (c) {
        '(' => self.makeToken(.TOKEN_LEFT_PAREN),
        ')' => self.makeToken(.TOKEN_RIGHT_PAREN),
        '{' => self.makeToken(.TOKEN_LEFT_BRACE),
        '}' => self.makeToken(.TOKEN_RIGHT_BRACE),
        ';' => self.makeToken(.TOKEN_SEMICOLON),
        ',' => self.makeToken(.TOKEN_COMMA),
        '.' => self.makeToken(.TOKEN_DOT),
        '-' => self.makeToken(.TOKEN_MINUS),
        '+' => self.makeToken(.TOKEN_PLUS),
        '/' => self.makeToken(.TOKEN_SLASH),
        '*' => self.makeToken(.TOKEN_STAR),
        '!' => self.makeToken(if (self.match('=')) .TOKEN_BANG_EQUAL else .TOKEN_BANG),
        '=' => self.makeToken(if (self.match('=')) .TOKEN_EQUAL_EQUAL else .TOKEN_EQUAL),
        '<' => self.makeToken(if (self.match('=')) .TOKEN_LESS_EQUAL else .TOKEN_LESS),
        '>' => self.makeToken(if (self.match('=')) .TOKEN_GREATER_EQUAL else .TOKEN_GREATER),
        '"' => self.makeString(c),
        '\'' => self.makeString(c),
        else => self.makeToken(.TOKEN_ERROR),
    };

    return token;
}

fn makeToken(self: *Scanner, token_type: TokenType) Token {
    var token = Token{
        .data = undefined,
        .type = token_type,
        .line = self.line,
        .length = self.cursor,
    };
    @memcpy(token.data[0..self.cursor], self.source[0..self.cursor]);
    self.moveSourceToCursor();
    return token;
}

fn makeString(self: *Scanner, delimiter: u8) Token {
    self.moveSourceToCursor();

    while (self.peek() != delimiter and !self.isAtEnd()) {
        _ = self.advance();
    }

    const token = self.makeToken(.TOKEN_STRING);
    _ = self.advance();
    self.moveSourceToCursor();

    return token;
}

fn makeNumber(self: *Scanner) Token {
    while (std.ascii.isDigit(self.peek())) {
        _ = self.advance();
    }

    const next = self.peekNext();
    if (self.peek() == '.' and next != null) {
        if (std.ascii.isDigit(next.?)) {
            _ = self.advance();
            while (std.ascii.isDigit(self.peek())) _ = self.advance();
        }
    }

    return self.makeToken(.TOKEN_NUMBER);
}

fn makeIdentifier(self: *Scanner) Token {
    while (std.ascii.isAlphanumeric(self.peek())) _ = self.advance();
    return self.makeToken(identifierType(self.source[0..self.cursor]));
}

fn identifierType(id: []const u8) TokenType {
    if (id.len <= 1) return .TOKEN_IDENTIFIER;

    const token_type = switch (id[0]) {
        'a' => checkKeyword(id[1..], "nd", .TOKEN_AND),
        'c' => checkKeyword(id[1..], "lass", .TOKEN_CLASS),
        'e' => checkKeyword(id[1..], "lse", .TOKEN_ELSE),
        'i' => checkKeyword(id[1..], "f", .TOKEN_IF),
        'n' => checkKeyword(id[1..], "il", .TOKEN_NIL),
        'o' => checkKeyword(id[1..], "r", .TOKEN_OR),
        'p' => checkKeyword(id[1..], "rint", .TOKEN_PRINT),
        'r' => checkKeyword(id[1..], "eturn", .TOKEN_RETURN),
        's' => checkKeyword(id[1..], "uper", .TOKEN_SUPER),
        'v' => checkKeyword(id[1..], "ar", .TOKEN_VAR),
        'w' => checkKeyword(id[1..], "hile", .TOKEN_WHILE),
        'f' => |_| t: {
            if (id.len <= 2) break :t .TOKEN_IDENTIFIER;
            break :t switch (id[1]) {
                'a' => checkKeyword(id[2..], "lse", .TOKEN_FALSE),
                'o' => checkKeyword(id[2..], "r", .TOKEN_FOR),
                'u' => checkKeyword(id[2..], "n", .TOKEN_FUN),
                else => .TOKEN_IDENTIFIER,
            };
        },
        't' => |_| t: {
            if (id.len <= 2) break :t .TOKEN_IDENTIFIER;
            break :t switch (id[1]) {
                'h' => checkKeyword(id[2..], "is", .TOKEN_THIS),
                'r' => checkKeyword(id[2..], "ue", .TOKEN_TRUE),
                else => .TOKEN_IDENTIFIER,
            };
        },
        else => .TOKEN_IDENTIFIER,
    };

    return token_type;
}

fn checkKeyword(id: []const u8, keyword: []const u8, token_type: TokenType) TokenType {
    if (id.len != keyword.len) return .TOKEN_IDENTIFIER;
    if (std.mem.eql(u8, id, keyword)) return token_type;
    return .TOKEN_IDENTIFIER;
}

// fn errorToken(self: *Scanner, msg: []const u8) Token {}

// helper funtions
fn isAtEnd(self: *Scanner) bool {
    return self.cursor + 1 >= self.source.len;
}

fn advance(self: *Scanner) u8 {
    const c = self.source[self.cursor];
    self.cursor += 1;
    return c;
}

fn match(self: *Scanner, char: u8) bool {
    if (self.isAtEnd()) return false;
    if (self.source[self.cursor] != char) return false;
    self.cursor += 1;
    return true;
}

fn peek(self: *Scanner) u8 {
    return self.source[self.cursor];
}

fn peekNext(self: *Scanner) ?u8 {
    if (self.isAtEnd()) return null;
    return self.source[self.cursor + 1];
}

fn moveSourceToCursor(self: *Scanner) void {
    self.source = self.source[self.cursor..];
    self.cursor = 0;
}

fn skipComment(self: *Scanner) void {
    if (self.peek() == '/' and self.peekNext() == '/') {
        while (self.peek() != '\n' and !self.isAtEnd()) {
            _ = self.advance();
        }

        self.line += 1;
        self.moveSourceToCursor();
    }
}

fn skipWhitespace(self: *Scanner) void {
    var is_space = std.ascii.isWhitespace(self.peek());
    if (!is_space) return;

    while (is_space) : (is_space = std.ascii.isWhitespace(self.peek())) {
        const c = self.advance();
        if (c == '\n') self.line += 1;
    }
    self.moveSourceToCursor();
}
