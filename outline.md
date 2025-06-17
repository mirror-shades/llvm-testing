IR to x86_64 Assembly Compiler Architecture
Overview

Prototype compiler from intermediate representation (IR) to x86_64 assembly, targeting a typed language with automatic reference counting (ARC) for strings and enum pattern matching capabilities.
Input: AST Characteristics Analysis

Based on provided AST sample:

    Complex expressions: Nested Binary, Logical, Match operations
    Type system: i32, f64, u8, bool, string, discriminant-only enums
    Control flow: If/Else, While, Match statements with pattern matching
    Functions: Named functions with parameters and local scopes
    Memory model: Value semantics with ARC for strings, fake references via value mapping

IR Design Decisions
Type System

    Primitive types: i32, f64, u8, bool, string
    Enum types: enum.<name> (simple discriminants, no associated data)
    Function types: func(<params>) -> <return_type>
    No explicit pointer types (matches language's value semantics)

IR Format: Typed Linear Three-Address Code (TAC)

<instruction> ::= <assignment> | <jump> | <call> | <return> | <label> | <arc_op>
<assignment> ::= <dest>:<type> = <op> <src1>:<type> [<src2>:<type>]
<jump> ::= goto <label> | if <condition>:<bool> goto <label>
<arc_op> ::= string_retain <src>:<string> | string_release <src>:<string>

Variable Representation

    Temporaries: %t1:<type>, %t2:<type>, ... for intermediate values
    Variables: %var_name:<type> preserving original names where possible
    Labels: L1, L2, ... for control flow targets

Memory Management Strategy

    Automatic ARC: Compiler generates explicit string_retain/string_release operations
    Integration with existing system: Leverage current ValueStorage.alias_count for ARC analysis
    String lifecycle: Create→Use→Automatic cleanup via generated release calls
    Primitives: Stack-allocated, no ARC needed

Output: Assembly Generation
Target Platform

    Architecture: x86_64 Linux
    Code type: Position-Independent Code (PIC) for flexibility
    Calling convention: System V AMD64 ABI
    Linking: Static linking with small runtime library

Zig Runtime Structure:

// runtime.zig
const std = @import("std");

export fn \_\_arc_string_create(data: [*:0]const u8, len: usize) [*:0]u8 {
// Implementation using your existing allocator approach
}

export fn \_\_arc_string_retain(str: [*:0]u8) void {
// Increment reference count
}

export fn \_\_arc_string_release(str: [*:0]u8) void {
// Decrement and potentially free
}

export fn \_\_runtime_print_string(str: [*:0]u8) void {
std.debug.print("{s}\n", .{str});
}

Assembly Structure

asm

.section .text
.global \_start

\_start: # Generated code with runtime calls
call **arc_string_create
call **runtime_print_string
call \_\_arc_string_release # Exit syscall
mov $60, %rax
mov $0, %rdi
syscall

Memory Layout

    Strings: Heap-allocated with reference counting metadata
    Primitives: Stack-allocated in function frames
    Enums: Stack-allocated as i32 discriminants
    Function locals: Standard stack frame layout

Key Compilation Phases

1. AST → IR Translation

   Flatten nested expressions into TAC sequence
   Generate temporaries for intermediate results
   Insert ARC operations based on existing alias_count analysis
   Transform control flow into explicit jumps and labels

2. IR → Assembly Generation

   Map IR operations to x86_64 instructions
   Handle register allocation (initially simple stack-based)
   Generate runtime library calls for string operations
   Emit standard function prologue/epilogue

3. Pattern Matching Compilation

   Match statements → Jump tables or conditional chains
   Enum discriminants → Integer comparisons
   Pattern guards → Additional conditional jumps

Prototype Scope
Minimal Viable Features

    String operations: Creation, printing, automatic cleanup
    Integer arithmetic: Add, subtract, multiply, divide, modulo
    Comparison operations: Equals, less than, greater than
    Control flow: If/else, while loops
    Enum matching: Basic pattern matching on discriminants
    Function calls: Parameter passing and return values

Output Artifacts

    IR text files: Human-readable intermediate representation
    Assembly files: .s files ready for assembler
    Runtime library: libruntime.a for linking
    Executable: Final linked binary

Technical Constraints
Register Usage Strategy

    Initial approach: Stack-based (all values on stack)
    Future optimization: Proper register allocation
    ABI compliance: Preserve caller-saved registers across calls

Error Handling

    IR validation: Type checking, undefined variable detection
    Assembly generation: Graceful degradation for unsupported operations
    Runtime errors: Basic assertions in runtime library

Integration Points

    Memory manager compatibility: Work with existing ARC system
    Scope management: Respect variable lifetime analysis
    Type system: Preserve type information through compilation

Success Criteria

Prototype successfully compiles simple programs featuring:

    String literals and printing
    Integer arithmetic and comparisons
    Basic control flow (if/while)
    Simple enum pattern matching
    Automatic string memory management
    Produces working x86_64 executables
