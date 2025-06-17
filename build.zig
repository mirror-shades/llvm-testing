const std = @import("std");

pub fn build(b: *std.Build) void {
    // Create the LLVM module
    const llvm_mod = b.createModule(.{
        .root_source_file = b.path("llvm/llvm-bindings.zig"),
    });

    // Create the main executable module
    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = b.standardTargetOptions(.{}),
        .optimize = b.standardOptimizeOption(.{}),
        .imports = &.{
            .{ .name = "llvm", .module = llvm_mod },
        },
    });

    const exe = b.addExecutable(.{
        .name = "main",
        .root_module = exe_mod,
    });

    // Add LLVM paths
    const llvm_path = "C:/Program Files/LLVM";
    exe.addIncludePath(.{ .cwd_relative = b.fmt("{s}/include", .{llvm_path}) });
    exe.addLibraryPath(.{ .cwd_relative = b.fmt("{s}/lib", .{llvm_path}) });
    exe.addLibraryPath(.{ .cwd_relative = b.fmt("{s}/bin", .{llvm_path}) });
    exe.addRPath(.{ .cwd_relative = b.fmt("{s}/bin", .{llvm_path}) });

    // Link core LLVM libraries - these are the actual DLLs available on Windows
    exe.linkSystemLibrary("LLVM-C");
    exe.linkSystemLibrary("libclang");
    exe.linkSystemLibrary("LTO");
    exe.linkSystemLibrary("Remarks");

    // Add Windows-specific libraries
    exe.linkSystemLibrary("ole32");
    exe.linkSystemLibrary("advapi32");
    exe.linkSystemLibrary("shell32");

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
