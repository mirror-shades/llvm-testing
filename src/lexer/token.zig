const std = @import("std");
const ast = @import("../ast/ast.zig");
const TokenLiteral = @import("../types/types.zig").TokenLiteral;

pub const TokenType = enum {
    // single-character tokens
    LEFT_BRACE, // {
    RIGHT_BRACE, // }
    LEFT_BRACKET, // [
    RIGHT_BRACKET, // ]
    COMMA, // ,
    SEMICOLON, // ;
    MODULO, // %
    HASH, // #
    TILDE, // ~
    INSPECT, // ?

    //one or two character tokens
    DOT, // .
    DOT_DOT, // ..
    AMPERSAND, // &
    ARROW, // =>
    SLASH, // /
    SLASH_EQUAL, // /=
    ASTERISK, // *
    ASTERISK_EQUAL, // *=
    POWER, // **
    POWER_EQUAL, // **=
    PLUS, // +
    PLUS_PLUS, // ++
    PLUS_EQUAL, // +=
    MINUS, // -
    MINUS_MINUS, // --
    MINUS_EQUAL, // -=
    MAIN, // ->
    BANG, // !
    BANG_EQUAL, // !=
    GREATER, // >
    GREATER_EQUAL, // >=
    LESS, // <
    LESS_EQUAL, // <=
    TYPE_SYMBOL, // ::
    LEFT_PAREN, // (
    RIGHT_PAREN, // )
    LEFT_TUPLE, // (:
    RIGHT_TUPLE, // :)

    // keywords
    VAR, // var
    CONST, // const
    IMPORT, // import
    PUBLIC, // public
    INPUT, // input
    ASSERT, // assert
    RETURN, // return
    RETURNS, // assign an expected return type
    BREAK, // break
    CONTINUE, // continue
    MATCH, // match
    THROW, // throw
    TRY, // try
    CATCH, // catch
    WHILE, // while
    FOR, // for
    FOREACH, // foreach
    XOR, // xor
    NOT_TRANCENDENTAL, // ⊖
    EXISTS, // exists ∃
    FORALL, // forall ∀
    FROM, // from
    IN, // in
    AS, // as
    NOT, // not ¬
    IMPLIES, // →
    ASYNC, // async
    AWAIT, // await
    TYPEOF, // typeof
    GUIDE, // guide
    SAFE, // safe
    NORMAL, // normal

    // keywords with alternate tokens
    AND, // and ∧
    OR, // or ∨
    FUNCTION, // fn
    WHERE, // where
    EQUALITY, // ==
    ASSIGN, // is

    // logical operators
    IF, // if
    IFF, // iff
    THEN, // then
    ELSE, // else
    NAND, // ↑
    NOR, // ↓

    // literals
    IDENTIFIER, // identifier
    SPREAD, // ...
    INT, // integer
    U8, // 8-bit unsigned integer
    FLOAT, // float
    STRING, // string
    ARRAY, // array
    TETRA, // tetra
    LOGIC, // logic
    STRUCT, // struct
    ENUM, // enum
    AUTO, // auto
    TUPLE, // tuple
    MAP, // map

    // Type keywords
    INT_TYPE, // int type declaration
    U8_TYPE, // 8-bit unsigned integer type declaration
    FLOAT_TYPE, // float type declaration
    STRING_TYPE, // string type declaration
    TETRA_TYPE, // tetra type declaration
    ARRAY_TYPE, // array type declaration
    STRUCT_TYPE, // struct type declaration
    ENUM_TYPE, // enum type declaration
    AUTO_TYPE, // auto type declaration
    TUPLE_TYPE, // tuple type declaration
    MAP_TYPE, // map type declaration
    NOTHING, // nothing

    EOF, // end of file
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    literal: TokenLiteral,
    line: i32,
    column: usize, // array index is usize by default

    pub fn init(token_type: TokenType, lexeme: []const u8, literal: TokenLiteral, line: i32, column: usize) Token {
        return Token{
            .type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
            .column = column, // array index is usize by default
        };
    }
};

pub fn deinit(self: *TokenLiteral, allocator: std.mem.Allocator) void {
    switch (self.*) {
        .map => |*m| {
            var iter = m.iterator();
            while (iter.next()) |entry| {
                var value = entry.value_ptr.*;
                value.deinit(allocator);
            }
            m.deinit();
        },
        else => {},
    }
}

// Add a helper function to convert TokenType to TypeInfo
pub fn convertTokenTypeToTypeInfo(token_type: TokenType) ast.TypeInfo {
    return switch (token_type) {
        .INT => ast.TypeInfo{ .base = .Int, .is_mutable = true, .is_dynamic = false },
        .U8 => ast.TypeInfo{ .base = .U8, .is_mutable = true, .is_dynamic = false },
        .FLOAT => ast.TypeInfo{ .base = .Float, .is_mutable = true, .is_dynamic = false },
        .STRING => ast.TypeInfo{ .base = .String, .is_mutable = true, .is_dynamic = false },
        .TETRA => ast.TypeInfo{ .base = .Tetra, .is_mutable = true, .is_dynamic = false },
        .ARRAY => ast.TypeInfo{ .base = .Array, .is_mutable = true, .is_dynamic = false },
        // Add more mappings as needed
        else => ast.TypeInfo{ .base = .Auto, .is_mutable = true, .is_dynamic = true },
    };
}
