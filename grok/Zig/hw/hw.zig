//! The customary hello world first program. This is a top-level comment.
//! I don't know if a program is considered a module or not.
const std = @import("std");

pub fn main() !void {
    // comments use 2 slashes, doc comments 3
    const stdout = std.io.getStdOut().writer();

    // A friendly greeting - zig does not like me making it a doc comment
    const hello = "Hello";
    try stdout.print("{s}, {s}!!!\n", .{ hello, "World" });
}
