const std = @import("std");
const llvm = @import("llvm");
const print_funcs = @import("print.zig");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMInit = llvm.initialization;
const LLVMTargetMachine = llvm.target_machine;
const LLVMTarget = llvm.target;

fn print(str: []const u8) void {
    std.debug.print("{s}", .{str});
}

fn printf(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
}

// ============================================================================
// OS Detection and Platform-Specific Configuration
// ============================================================================

const TargetOS = enum {
    windows,
    linux,
    macos,
};

const current_os: TargetOS = switch (@import("builtin").os.tag) {
    .windows => .windows,
    .macos => .macos,
    else => .linux, // Default to Linux for other Unix-like systems
};

// Add this near the top where other constants are defined
const obj_extension = if (current_os == .windows) ".obj" else ".o";

// ============================================================================
// IR Type System - Scalable for future expansion
// ============================================================================

const IRType = enum {
    i32,
    i64,
    f64,
    string,
    bool,
    // Future: enum types, function types, etc.

    pub fn format(self: IRType, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll(@tagName(self));
    }
};

const IRValue = union(enum) {
    i32: i32,
    i64: i64,
    f64: f64,
    string: []const u8,
    bool: bool,
};

const BinaryOp = enum {
    add,
    sub,
    mul,
    div,
    mod,
    // Future: comparison ops, logical ops, etc.
};

const TempId = u32;

// ============================================================================
// IR Instruction Set - Designed for easy expansion
// ============================================================================

const IRInstruction = union(enum) {
    // Value operations
    load_immediate: struct { dest: TempId, value: IRValue },

    // Arithmetic operations
    binary_op: struct { dest: TempId, op: BinaryOp, lhs: TempId, rhs: TempId },

    // String operations
    string_literal: struct { dest: TempId, data: []const u8 },
    string_from_int: struct { dest: TempId, src: TempId },

    // I/O operations
    print_string: struct { src: TempId },
    print_int: struct { src: TempId },

    // Control flow (for future expansion)
    // label: struct { name: []const u8 },
    // jump: struct { target: []const u8 },
    // conditional_jump: struct { condition: TempId, target: []const u8 },

    // Memory management (for future ARC)
    // string_retain: struct { src: TempId },
    // string_release: struct { src: TempId },

    // Program structure
    program_end,
};

// ============================================================================
// IR Program Structure
// ============================================================================

const IRProgram = struct {
    instructions: []IRInstruction,
    temp_types: []IRType,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *IRProgram) void {
        for (self.instructions) |instruction| {
            switch (instruction) {
                .string_literal => |str_lit| {
                    self.allocator.free(str_lit.data);
                },
                else => {},
            }
        }
        self.allocator.free(self.instructions);
        self.allocator.free(self.temp_types);
    }
};

// ============================================================================
// IR Parser - Converts text format to structured IR
// ============================================================================

const ParseError = error{
    InvalidInstruction,
    InvalidTemp,
    InvalidType,
    InvalidValue,
    UnexpectedToken,
    OutOfMemory,
};

const Parser = struct {
    allocator: std.mem.Allocator,

    pub fn parseProgram(self: *Parser, input: []const u8) ParseError!IRProgram {
        var instructions = std.ArrayList(IRInstruction).init(self.allocator);
        errdefer instructions.deinit();

        var temp_types = std.ArrayList(IRType).init(self.allocator);
        errdefer temp_types.deinit();

        var max_temp_id: u32 = 0;

        var lines = std.mem.splitScalar(u8, input, '\n');
        while (lines.next()) |line| {
            const trimmed = std.mem.trim(u8, line, " \t\r");
            if (trimmed.len == 0 or trimmed[0] == '#') continue; // Skip empty lines and comments

            const instruction = try self.parseLine(trimmed, &max_temp_id);
            try instructions.append(instruction);
        }

        // Initialize temp_types array
        try temp_types.resize(max_temp_id + 1);
        for (temp_types.items) |*temp_type| {
            temp_type.* = .i32; // Default type, will be refined during parsing
        }

        return IRProgram{
            .instructions = try instructions.toOwnedSlice(),
            .temp_types = try temp_types.toOwnedSlice(),
            .allocator = self.allocator,
        };
    }

    fn parseLine(self: *Parser, line: []const u8, max_temp_id: *u32) ParseError!IRInstruction {
        var tokens = std.mem.tokenizeAny(u8, line, " \t");

        const first_token = tokens.next() orelse return ParseError.UnexpectedToken;

        if (std.mem.eql(u8, first_token, "load_immediate")) {
            const dest_str = tokens.next() orelse return ParseError.UnexpectedToken;
            const value_str = tokens.next() orelse return ParseError.UnexpectedToken;

            const dest = try self.parseTemp(dest_str, max_temp_id);
            const value = try self.parseValue(value_str);

            return IRInstruction{ .load_immediate = .{ .dest = dest.id, .value = value } };
        } else if (std.mem.eql(u8, first_token, "binary_op")) {
            const dest_str = tokens.next() orelse return ParseError.UnexpectedToken;
            const op_str = tokens.next() orelse return ParseError.UnexpectedToken;
            const lhs_str = tokens.next() orelse return ParseError.UnexpectedToken;
            const rhs_str = tokens.next() orelse return ParseError.UnexpectedToken;

            const dest = try self.parseTemp(dest_str, max_temp_id);
            const op = try self.parseBinaryOp(op_str);
            const lhs = try self.parseTemp(lhs_str, max_temp_id);
            const rhs = try self.parseTemp(rhs_str, max_temp_id);

            return IRInstruction{ .binary_op = .{ .dest = dest.id, .op = op, .lhs = lhs.id, .rhs = rhs.id } };
        } else if (std.mem.eql(u8, first_token, "string_literal")) {
            const dest_str = tokens.next() orelse return ParseError.UnexpectedToken;

            // Find the start of the string literal (first quote)
            var remaining = tokens.rest();
            const quote_pos = std.mem.indexOfScalar(u8, remaining, '"') orelse return ParseError.InvalidValue;
            remaining = remaining[quote_pos..];

            // Find the end of the string literal (last quote)
            const end_quote_pos = std.mem.lastIndexOfScalar(u8, remaining, '"') orelse return ParseError.InvalidValue;
            const data_str = remaining[0 .. end_quote_pos + 1];

            const dest = try self.parseTemp(dest_str, max_temp_id);
            const data = try self.parseStringLiteral(data_str);

            return IRInstruction{ .string_literal = .{ .dest = dest.id, .data = data } };
        } else if (std.mem.eql(u8, first_token, "string_from_int")) {
            const dest_str = tokens.next() orelse return ParseError.UnexpectedToken;
            const src_str = tokens.next() orelse return ParseError.UnexpectedToken;

            const dest = try self.parseTemp(dest_str, max_temp_id);
            const src = try self.parseTemp(src_str, max_temp_id);

            return IRInstruction{ .string_from_int = .{ .dest = dest.id, .src = src.id } };
        } else if (std.mem.eql(u8, first_token, "print_string")) {
            const src_str = tokens.next() orelse return ParseError.UnexpectedToken;
            const src = try self.parseTemp(src_str, max_temp_id);

            return IRInstruction{ .print_string = .{ .src = src.id } };
        } else if (std.mem.eql(u8, first_token, "print_int")) {
            const src_str = tokens.next() orelse return ParseError.UnexpectedToken;
            const src = try self.parseTemp(src_str, max_temp_id);

            return IRInstruction{ .print_int = .{ .src = src.id } };
        } else if (std.mem.eql(u8, first_token, "program_end")) {
            return IRInstruction.program_end;
        } else {
            return ParseError.InvalidInstruction;
        }
    }

    const TempInfo = struct {
        id: TempId,
        type: IRType,
    };

    fn parseTemp(self: *Parser, temp_str: []const u8, max_temp_id: *u32) ParseError!TempInfo {
        _ = self;

        if (temp_str.len < 2 or temp_str[0] != '%') {
            return ParseError.InvalidTemp;
        }

        // Parse %0:i32 format
        var parts = std.mem.splitScalar(u8, temp_str[1..], ':');
        const id_str = parts.next() orelse return ParseError.InvalidTemp;
        const type_str = parts.next() orelse return ParseError.InvalidTemp;

        const id = std.fmt.parseInt(u32, id_str, 10) catch return ParseError.InvalidTemp;
        const temp_type = std.meta.stringToEnum(IRType, type_str) orelse return ParseError.InvalidType;

        max_temp_id.* = @max(max_temp_id.*, id);

        return TempInfo{ .id = id, .type = temp_type };
    }

    fn parseValue(self: *Parser, value_str: []const u8) ParseError!IRValue {
        _ = self;

        // Try parsing as integer first
        if (std.fmt.parseInt(i32, value_str, 10)) |int_val| {
            return IRValue{ .i32 = int_val };
        } else |_| {}

        // Try parsing as float
        if (std.fmt.parseFloat(f64, value_str)) |float_val| {
            return IRValue{ .f64 = float_val };
        } else |_| {}

        return ParseError.InvalidValue;
    }

    fn parseBinaryOp(self: *Parser, op_str: []const u8) ParseError!BinaryOp {
        _ = self;
        return std.meta.stringToEnum(BinaryOp, op_str) orelse ParseError.InvalidValue;
    }

    fn parseStringLiteral(self: *Parser, data_str: []const u8) ParseError![]const u8 {
        // Remove quotes if present
        if (data_str.len >= 2 and data_str[0] == '"' and data_str[data_str.len - 1] == '"') {
            const str = data_str[1 .. data_str.len - 1];
            // Make a copy of the string
            const owned_str = try self.allocator.dupe(u8, str);
            return owned_str;
        }
        // Make a copy of the string
        return try self.allocator.dupe(u8, data_str);
    }
};

// ============================================================================
// LLVM Generator - Converts IR to LLVM IR
// ============================================================================

const LLVMGenerator = struct {
    allocator: std.mem.Allocator,
    context: LLVMTypes.LLVMContextRef,
    module: LLVMTypes.LLVMModuleRef,
    builder: LLVMTypes.LLVMBuilderRef,
    print_string_fn: LLVMTypes.LLVMValueRef,
    print_string_fn_type: LLVMTypes.LLVMTypeRef,
    print_int_fn: LLVMTypes.LLVMValueRef,
    print_int_fn_type: LLVMTypes.LLVMTypeRef,
    temp_values: std.ArrayList(LLVMTypes.LLVMValueRef),
    string_literals: std.ArrayList([]const u8),
    target_machine: LLVMTypes.LLVMTargetMachineRef,

    pub fn init(allocator: std.mem.Allocator) !LLVMGenerator {
        // Create context, module, and builder
        const context = LLVMCore.LLVMContextCreate();
        errdefer LLVMCore.LLVMContextDispose(context);

        const module = LLVMCore.LLVMModuleCreateWithNameInContext("main", context);
        errdefer LLVMCore.LLVMDisposeModule(module);

        const builder = LLVMCore.LLVMCreateBuilderInContext(context);
        errdefer LLVMCore.LLVMDisposeBuilder(builder);

        // Create print functions
        var print_string_fn_type: LLVMTypes.LLVMTypeRef = undefined;
        const print_string_fn = try createPrintStringFunction(context, module, &print_string_fn_type);

        var print_int_fn_type: LLVMTypes.LLVMTypeRef = undefined;
        const print_int_fn = try createPrintIntFunction(context, module, &print_int_fn_type);

        // Get target machine
        const target_triple = LLVMTargetMachine.LLVMGetDefaultTargetTriple();
        if (target_triple == null) {
            return error.TargetTripleInitializationFailed;
        }
        defer LLVMCore.LLVMDisposeMessage(target_triple);

        var target: LLVMTypes.LLVMTargetRef = undefined;
        var error_message: [*c]u8 = undefined;
        if (LLVMTargetMachine.LLVMGetTargetFromTriple(target_triple, &target, &error_message) != 0) {
            printf("Error getting target: {s}\n", .{error_message});
            LLVMCore.LLVMDisposeMessage(error_message);
            return error.TargetInitializationFailed;
        }

        const cpu = LLVMTargetMachine.LLVMGetHostCPUName();
        if (cpu == null) {
            return error.CPUNameInitializationFailed;
        }
        defer LLVMCore.LLVMDisposeMessage(cpu);

        const features = LLVMTargetMachine.LLVMGetHostCPUFeatures();
        if (features == null) {
            return error.CPUFeaturesInitializationFailed;
        }
        defer LLVMCore.LLVMDisposeMessage(features);

        const target_machine = LLVMTargetMachine.LLVMCreateTargetMachine(
            target,
            target_triple,
            cpu,
            features,
            LLVMTypes.LLVMCodeGenOptLevel.LLVMCodeGenLevelDefault,
            LLVMTypes.LLVMRelocMode.LLVMRelocDefault,
            LLVMTypes.LLVMCodeModel.LLVMCodeModelDefault,
        );
        if (target_machine == null) {
            return error.TargetMachineCreationFailed;
        }
        errdefer LLVMTargetMachine.LLVMDisposeTargetMachine(target_machine);

        // Set target triple and data layout
        LLVMCore.LLVMSetTarget(module, target_triple);
        const data_layout = LLVMTargetMachine.LLVMCreateTargetDataLayout(target_machine);
        defer LLVMTarget.LLVMDisposeTargetData(data_layout);
        const data_layout_str = LLVMTarget.LLVMCopyStringRepOfTargetData(data_layout);
        defer LLVMCore.LLVMDisposeMessage(data_layout_str);
        LLVMCore.LLVMSetDataLayout(module, data_layout_str);

        var temp_values = std.ArrayList(LLVMTypes.LLVMValueRef).init(allocator);
        errdefer temp_values.deinit();

        var string_literals = std.ArrayList([]const u8).init(allocator);
        errdefer {
            for (string_literals.items) |str| {
                allocator.free(str);
            }
            string_literals.deinit();
        }

        return LLVMGenerator{
            .allocator = allocator,
            .context = context,
            .module = module,
            .builder = builder,
            .print_string_fn = print_string_fn,
            .print_string_fn_type = print_string_fn_type,
            .print_int_fn = print_int_fn,
            .print_int_fn_type = print_int_fn_type,
            .temp_values = temp_values,
            .string_literals = string_literals,
            .target_machine = target_machine,
        };
    }

    pub fn deinit(self: *LLVMGenerator) void {
        // Clean up LLVM resources
        LLVMCore.LLVMDisposeBuilder(self.builder);
        LLVMCore.LLVMDisposeModule(self.module);
        LLVMCore.LLVMContextDispose(self.context);
        LLVMTargetMachine.LLVMDisposeTargetMachine(self.target_machine);

        // Clean up string literals
        for (self.string_literals.items) |str| {
            // These strings are owned by the Parser, don't free them here
            _ = str;
        }
        self.string_literals.deinit();
        self.temp_values.deinit();
    }

    pub fn generateLLVM(self: *LLVMGenerator, program: IRProgram) ![]u8 {
        // Create main function
        const main_fn_type = LLVMCore.LLVMFunctionType(
            LLVMCore.LLVMInt32TypeInContext(self.context),
            null,
            0,
            @intFromBool(false),
        );
        const main_fn = LLVMCore.LLVMAddFunction(self.module, "main", main_fn_type);
        const entry_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, main_fn, "entry");
        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, entry_block);

        // Initialize temp_values array with null values
        try self.temp_values.resize(program.temp_types.len);
        for (self.temp_values.items) |*val| {
            val.* = null;
        }

        // Generate code for each instruction
        for (program.instructions, 0..) |instruction, i| {
            printf("Generating instruction {d}...\n", .{i});
            try self.generateInstruction(instruction);
        }

        // Add return 0
        _ = LLVMCore.LLVMBuildRet(self.builder, LLVMCore.LLVMConstInt(
            LLVMCore.LLVMInt32TypeInContext(self.context),
            0,
            @intFromBool(false),
        ));

        // Get LLVM IR as string
        const ir_string = LLVMCore.LLVMPrintModuleToString(self.module);
        if (ir_string == null) {
            return error.IRGenerationFailed;
        }
        defer LLVMCore.LLVMDisposeMessage(ir_string);

        // Convert to Zig string
        return self.allocator.dupe(u8, std.mem.span(ir_string));
    }

    pub fn generateInstruction(self: *LLVMGenerator, instruction: IRInstruction) !void {
        switch (instruction) {
            .load_immediate => |load| {
                switch (load.value) {
                    .i32 => |val| {
                        const const_val = LLVMCore.LLVMConstInt(
                            LLVMCore.LLVMInt32TypeInContext(self.context),
                            @intCast(val),
                            @intFromBool(true),
                        );
                        if (load.dest >= self.temp_values.items.len) {
                            return error.InvalidTempId;
                        }
                        self.temp_values.items[load.dest] = const_val;
                    },
                    .i64 => |val| {
                        const const_val = LLVMCore.LLVMConstInt(
                            LLVMCore.LLVMInt64TypeInContext(self.context),
                            @intCast(val),
                            @intFromBool(true),
                        );
                        if (load.dest >= self.temp_values.items.len) {
                            return error.InvalidTempId;
                        }
                        self.temp_values.items[load.dest] = const_val;
                    },
                    else => return error.UnsupportedValue,
                }
            },

            .binary_op => |binop| {
                if (binop.lhs >= self.temp_values.items.len or binop.rhs >= self.temp_values.items.len) {
                    return error.InvalidTempId;
                }
                const lhs = self.temp_values.items[binop.lhs];
                const rhs = self.temp_values.items[binop.rhs];

                if (lhs == null or rhs == null) {
                    return error.UninitializedValue;
                }

                const result = switch (binop.op) {
                    .add => LLVMCore.LLVMBuildAdd(self.builder, lhs, rhs, "add"),
                    .sub => LLVMCore.LLVMBuildSub(self.builder, lhs, rhs, "sub"),
                    .mul => LLVMCore.LLVMBuildMul(self.builder, lhs, rhs, "mul"),
                    .div => LLVMCore.LLVMBuildSDiv(self.builder, lhs, rhs, "div"),
                    .mod => LLVMCore.LLVMBuildSRem(self.builder, lhs, rhs, "mod"),
                };

                if (binop.dest >= self.temp_values.items.len) {
                    return error.InvalidTempId;
                }
                self.temp_values.items[binop.dest] = result;
            },

            .string_literal => |str_lit| {
                // Create a null-terminated copy of the string for LLVM
                var buffer: [256]u8 = undefined;
                const len = @min(str_lit.data.len, buffer.len - 1);
                @memcpy(buffer[0..len], str_lit.data[0..len]);
                buffer[len] = 0;

                // Create a global string constant
                const str_val = LLVMCore.LLVMConstStringInContext(
                    self.context,
                    @ptrCast(&buffer),
                    @intCast(len),
                    @intFromBool(false), // Don't null terminate the LLVM string constant
                );

                const global_str = LLVMCore.LLVMAddGlobal(
                    self.module,
                    LLVMCore.LLVMTypeOf(str_val),
                    "str",
                );
                LLVMCore.LLVMSetInitializer(global_str, str_val);
                LLVMCore.LLVMSetGlobalConstant(global_str, @intFromBool(true));
                LLVMCore.LLVMSetLinkage(global_str, LLVMTypes.LLVMLinkage.LLVMPrivateLinkage);

                if (str_lit.dest >= self.temp_values.items.len) {
                    return error.InvalidTempId;
                }
                self.temp_values.items[str_lit.dest] = global_str;

                // Store the string for cleanup
                try self.string_literals.append(str_lit.data);
            },

            .string_from_int => |str_int| {
                // Call runtime function to convert int to string
                const int_val = self.temp_values.items[str_int.src];
                if (int_val == null) {
                    return error.UninitializedValue;
                }
                var args = [_]LLVMTypes.LLVMValueRef{int_val};
                const result = LLVMCore.LLVMBuildCall2(
                    self.builder,
                    LLVMCore.LLVMTypeOf(self.print_int_fn),
                    self.print_int_fn,
                    @ptrCast(&args),
                    1,
                    "int_to_str",
                );
                if (str_int.dest >= self.temp_values.items.len) {
                    return error.InvalidTempId;
                }
                self.temp_values.items[str_int.dest] = result;
            },

            .print_string => |print_str| {
                if (print_str.src >= self.temp_values.items.len) {
                    return error.InvalidTempId;
                }
                const str_val = self.temp_values.items[print_str.src];
                if (str_val == null) {
                    return error.UninitializedValue;
                }

                // Create a pointer to the string
                const str_ptr = LLVMCore.LLVMBuildBitCast(
                    self.builder,
                    str_val,
                    LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(self.context), 0),
                    "str_ptr",
                );

                var args = [_]LLVMTypes.LLVMValueRef{str_ptr};
                _ = LLVMCore.LLVMBuildCall2(
                    self.builder,
                    self.print_string_fn_type,
                    self.print_string_fn,
                    &args,
                    1,
                    "",
                );
            },

            .print_int => |print_int| {
                if (print_int.src >= self.temp_values.items.len) {
                    return error.InvalidTempId;
                }
                const int_val = self.temp_values.items[print_int.src];
                if (int_val == null) {
                    return error.UninitializedValue;
                }

                var args = [_]LLVMTypes.LLVMValueRef{int_val};
                _ = LLVMCore.LLVMBuildCall2(
                    self.builder,
                    self.print_int_fn_type,
                    self.print_int_fn,
                    &args,
                    1,
                    "",
                );
            },

            .program_end => {},
        }
    }

    fn createPrintStringFunction(context: LLVMTypes.LLVMContextRef, module: LLVMTypes.LLVMModuleRef, out_type: *LLVMTypes.LLVMTypeRef) !LLVMTypes.LLVMValueRef {
        // Create function type: void print_string(i8*)
        const i8_type = LLVMCore.LLVMInt8TypeInContext(context);
        const i8_ptr_type = LLVMCore.LLVMPointerType(i8_type, 0);
        var param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_type};

        const fn_type = LLVMCore.LLVMFunctionType(
            LLVMCore.LLVMVoidTypeInContext(context),
            &param_types,
            1,
            @intFromBool(false),
        );
        out_type.* = fn_type;

        // Create function declaration - use the exact name from print.zig
        const fn_val = LLVMCore.LLVMAddFunction(module, "print_string", fn_type);
        LLVMCore.LLVMSetLinkage(fn_val, LLVMTypes.LLVMLinkage.LLVMExternalLinkage);
        return fn_val;
    }

    fn createPrintIntFunction(context: LLVMTypes.LLVMContextRef, module: LLVMTypes.LLVMModuleRef, out_type: *LLVMTypes.LLVMTypeRef) !LLVMTypes.LLVMValueRef {
        // Create function type: void print_int(i32)
        const i32_type = LLVMCore.LLVMInt32TypeInContext(context);
        var param_types = [_]LLVMTypes.LLVMTypeRef{i32_type};

        const fn_type = LLVMCore.LLVMFunctionType(
            LLVMCore.LLVMVoidTypeInContext(context),
            &param_types,
            1,
            @intFromBool(false),
        );
        out_type.* = fn_type;

        // Create function declaration - use the exact name from print.zig
        const fn_val = LLVMCore.LLVMAddFunction(module, "print_int", fn_type);
        LLVMCore.LLVMSetLinkage(fn_val, LLVMTypes.LLVMLinkage.LLVMExternalLinkage);
        return fn_val;
    }

    pub fn emitObjectCode(self: *LLVMGenerator, output_path: []const u8) !void {
        var error_message: [*c]u8 = undefined;
        if (LLVMTargetMachine.LLVMTargetMachineEmitToFile(
            self.target_machine,
            self.module,
            output_path.ptr,
            LLVMTypes.LLVMCodeGenFileType.LLVMObjectFile,
            &error_message,
        ) != 0) {
            printf("Error emitting object code: {s}\n", .{error_message});
            LLVMCore.LLVMDisposeMessage(error_message);
            return error.EmitObjectCodeFailed;
        }
    }
};

// ============================================================================
// Main Compiler Driver
// ============================================================================

pub fn main() !void {
    // Initialize LLVM targets
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

    // Get target machine
    const target_triple = LLVMTargetMachine.LLVMGetDefaultTargetTriple();
    if (target_triple == null) {
        return error.TargetTripleInitializationFailed;
    }
    defer LLVMCore.LLVMDisposeMessage(target_triple);

    var target: LLVMTypes.LLVMTargetRef = undefined;
    var error_message: [*c]u8 = undefined;
    if (LLVMTargetMachine.LLVMGetTargetFromTriple(target_triple, &target, &error_message) != 0) {
        printf("Error getting target: {s}\n", .{error_message});
        LLVMCore.LLVMDisposeMessage(error_message);
        return error.TargetInitializationFailed;
    }

    // Create target machine
    const cpu = LLVMTargetMachine.LLVMGetHostCPUName();
    if (cpu == null) {
        return error.CPUNameInitializationFailed;
    }
    defer LLVMCore.LLVMDisposeMessage(cpu);

    const features = LLVMTargetMachine.LLVMGetHostCPUFeatures();
    if (features == null) {
        return error.CPUFeaturesInitializationFailed;
    }
    defer LLVMCore.LLVMDisposeMessage(features);

    const target_machine = LLVMTargetMachine.LLVMCreateTargetMachine(
        target,
        target_triple,
        cpu,
        features,
        LLVMTypes.LLVMCodeGenOptLevel.LLVMCodeGenLevelDefault,
        LLVMTypes.LLVMRelocMode.LLVMRelocDefault,
        LLVMTypes.LLVMCodeModel.LLVMCodeModelDefault,
    );
    if (target_machine == null) {
        return error.TargetMachineCreationFailed;
    }
    defer LLVMTargetMachine.LLVMDisposeTargetMachine(target_machine);

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Example IR program - in real use this would come from a file
    const example_ir =
        \\# Example IR program: integer arithmetic and string printing
        \\load_immediate %0:i32 42
        \\load_immediate %1:i32 10
        \\binary_op %2:i32 add %0:i32 %1:i32
        \\binary_op %3:i32 mul %2:i32 %0:i32
        \\string_literal %4:string "Hello from IR compiler!"
        \\print_string %4:string
        \\print_int %3:i32
        \\program_end
    ;

    print("IR Compiler - LLVM Backend\n");
    print("=========================\n\n");

    // Parse IR
    var parser = Parser{ .allocator = allocator };
    var program = parser.parseProgram(example_ir) catch |err| {
        printf("Parse error: {}\n", .{err});
        return err;
    };
    defer program.deinit();

    printf("Parsed {} instructions\n", .{program.instructions.len});

    // Generate LLVM IR
    var generator = LLVMGenerator.init(allocator) catch |err| {
        printf("LLVM initialization error: {}\n", .{err});
        return err;
    };
    defer generator.deinit();

    const llvm_ir = generator.generateLLVM(program) catch |err| {
        printf("LLVM IR generation error: {}\n", .{err});
        return err;
    };
    defer allocator.free(llvm_ir);

    print("Generated LLVM IR:\n");
    print("=================\n");
    printf("{s}\n", .{llvm_ir});

    // Write LLVM IR to file
    const file = std.fs.cwd().createFile("output.ll", .{}) catch |err| {
        printf("Could not create output file: {}\n", .{err});
        return err;
    };
    defer file.close();

    file.writeAll(llvm_ir) catch |err| {
        printf("Could not write to output file: {}\n", .{err});
        return err;
    };
    print("\nLLVM IR written to output.ll\n");

    // Generate object code
    const output_path = "output" ++ obj_extension;
    generator.emitObjectCode(output_path) catch |err| {
        printf("Could not emit object code: {}\n", .{err});
        return err;
    };
    printf("\nObject code written to {s}\n", .{output_path});
    printf("To build: clang {s} -o output\n", .{output_path});
}
