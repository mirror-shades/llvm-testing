const std = @import("std");
const llvm = @import("llvm");
const ast = @import("ast/ast.zig");
const LLVMGenerator = @import("codegen/llvm.zig").LLVMGenerator;

const obj_extension = if (@import("builtin").os.tag == .windows) ".obj" else ".o";

pub fn main() !void {
    // Initialize LLVM targets
    try initializeLLVM();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Example AST - in real use this would come from your parser
    const example_ast = try createExampleAST(allocator);
    defer {
        example_ast.deinit(allocator);
        allocator.destroy(example_ast);
    }

    // Initialize the LLVM generator
    var generator = try LLVMGenerator.init(allocator);
    defer generator.deinit();

    // Generate LLVM IR from AST
    try generator.generateAST(example_ast);

    // Write LLVM IR to file
    const ir_content = try generator.emitLLVMIR("output.ll");
    defer allocator.free(ir_content);
    print("\nLLVM IR written to output.ll\n");

    // Generate object code
    try generator.emitObjectCode("output" ++ obj_extension);
    printf("\nObject code written to {s}\n", .{"output" ++ obj_extension});
    if (@import("builtin").os.tag == .windows) {
        printf("To build: clang -target x86_64-w64-mingw32 {s} -o test.exe\n", .{"output" ++ obj_extension});
    } else {
        printf("To build: clang {s} -o test\n", .{"output" ++ obj_extension});
    }
}

fn initializeLLVM() !void {
    const llvm_target = @import("llvm").target;
    llvm_target.LLVMInitializeAllTargetInfos();
    llvm_target.LLVMInitializeAllTargets();
    llvm_target.LLVMInitializeAllTargetMCs();
    llvm_target.LLVMInitializeAllAsmParsers();
    llvm_target.LLVMInitializeAllAsmPrinters();

    // Initialize native target
    if (llvm_target.LLVMInitializeNativeTarget() != 0) {
        return error.NativeTargetInitializationFailed;
    }
    if (llvm_target.LLVMInitializeNativeAsmParser() != 0) {
        return error.NativeAsmParserInitializationFailed;
    }
    if (llvm_target.LLVMInitializeNativeAsmPrinter() != 0) {
        return error.NativeAsmPrinterInitializationFailed;
    }
    if (llvm_target.LLVMInitializeNativeDisassembler() != 0) {
        return error.NativeDisassemblerInitializationFailed;
    }
}

fn createExampleAST(allocator: std.mem.Allocator) !*ast.Expr {
    // Create the statements array
    var statements = try allocator.alloc(ast.Stmt, 2);
    statements[0] = .{ .Expression = try createPrintCall(allocator, "Hello, World!") };
    statements[1] = .{ .Return = .{
        .value = try createIntLiteral(allocator, 0),
        .type_info = .{ .base = .Int },
    } };

    // Create a function that will serve as our main
    const main_func = try allocator.create(ast.Expr);
    main_func.* = ast.Expr{ .Function = .{
        .name = .{
            .type = .IDENTIFIER,
            .lexeme = "main",
            .literal = .{ .nothing = {} },
            .line = 1,
            .column = 1,
        },
        .params = &[_]ast.FunctionParam{},
        .return_type_info = .{ .base = .Int },
        .body = statements,
        .is_entry = true,
    } };
    return main_func;
}

fn createPrintCall(allocator: std.mem.Allocator, text: []const u8) !*ast.Expr {
    // Create a string literal
    const str_literal = try allocator.create(ast.Expr);
    str_literal.* = ast.Expr{ .Literal = .{ .string = try allocator.dupe(u8, text) } };

    // Create an inspect expression to print it
    const inspect = try allocator.create(ast.Expr);
    inspect.* = ast.Expr{ .Inspect = .{
        .expr = str_literal,
        .location = .{
            .file = "main",
            .line = 1,
            .column = 1,
        },
        .variable_name = null,
    } };
    return inspect;
}

fn createIntLiteral(allocator: std.mem.Allocator, value: i32) !*ast.Expr {
    const literal = try allocator.create(ast.Expr);
    literal.* = ast.Expr{ .Literal = .{ .int = value } };
    return literal;
}

fn print(str: []const u8) void {
    std.debug.print("{s}", .{str});
}

fn printf(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
}
