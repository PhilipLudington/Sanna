pub const Token = @import("Token.zig");
pub const Lexer = @import("Lexer.zig");

pub const TokenType = Token.TokenType;
pub const Location = Token.Location;
pub const Span = Token.Span;

test {
    _ = Token;
    _ = Lexer;
}
