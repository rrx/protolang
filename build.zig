const std = @import("std");
const Builder = @import("std").build.Builder;
const ArrayList = std.ArrayList;

pub fn build(b: *Builder) !void {
  const allocator = std.heap.page_allocator;

  const cflags = try std.fmt.allocPrint(
        allocator,
        "-fno-direct-access-external-data {s}",
        .{ std.os.getenv("NIX_CFLAGS_COMPILE") },
      );
  defer allocator.free(cflags);

  var list = ArrayList([]const u8).init(allocator);
  defer list.deinit();
  var iter = std.mem.split(u8, cflags, " ");

  while (iter.next()) |s| {
      try list.append(s);
  }

  const mode = b.standardReleaseOptions();
  const target = b.standardTargetOptions(.{});

  const shared = b.addSharedLibrary("live", "link/testfiles/live.c", .unversioned);
  shared.setBuildMode(mode);
  shared.force_pic = true;
  shared.setTarget(target);
  shared.install();

  const lib = b.addStaticLibrary("example", null);
  lib.addCSourceFile("link/testfiles/print_stuff.c", list.items);
  lib.addCSourceFile("link/testfiles/print_string.c", list.items);
  lib.addCSourceFile("link/testfiles/testfunction.c", list.items);
  lib.addCSourceFile("link/testfiles/simplefunction.c", list.items);
  lib.addCSourceFile("link/testfiles/asdf.c", list.items);
  lib.addCSourceFile("link/testfiles/live.c", list.items);
  lib.addCSourceFile("link/testfiles/empty_main.c", list.items);
  lib.addCSourceFile("link/testfiles/uvtest.c", list.items);
  lib.addCSourceFile("link/testfiles/globals.c", list.items);
  lib.addCSourceFile("link/testfiles/call_extern.c", list.items);
  lib.setBuildMode(mode);
  lib.force_pic = true;
  lib.setTarget(target);
  lib.install();

  const main = b.addExecutable("empty_main", "link/testfiles/empty_main.c");
  main.setBuildMode(mode);
  main.force_pic = true;
  main.setTarget(target);
  main.linkLibC();
  main.install();

}
