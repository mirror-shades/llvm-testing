const std = @import("std");

extern "c" fn puts(str: [*:0]const u8) c_int;
extern "c" fn printf(format: [*:0]const u8, ...) c_int;

pub export fn print_string(str: [*:0]const u8) callconv(.C) void {
    _ = puts(str);
}

pub export fn print_int(num: i32) callconv(.C) void {
    _ = printf("%d\n", num);
}
