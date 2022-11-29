{
  description = "A Nix-flake-based Rust development environment";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , rust-overlay
    }:

    flake-utils.lib.eachDefaultSystem (system:
    let
      overlays = [
        (import rust-overlay)
        (self: super: {
          rustToolchain =
            let
              rust = super.rust-bin;
            in
            if builtins.pathExists ./rust-toolchain.toml then
              rust.fromRustupToolchainFile ./rust-toolchain.toml
            else if builtins.pathExists ./rust-toolchain then
              rust.fromRustupToolchainFile ./rust-toolchain
            else
              rust.stable.latest.default;
        })
      ];

      pkgs = import nixpkgs { inherit system overlays; };
    in
    {
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          rustToolchain
          openssl
          pkg-config
          cargo-deny
          cargo-edit
          cargo-watch
          rust-analyzer
          git
          llvmPackages_13.clang
          llvmPackages_13.llvm.dev
          fzf
          zig
          pkg-config
          rust-bindgen
          cargo-criterion
          bazel_5
          libsigsegv
          libuv
          glibc
          libffi
          libxml2
          #musl
        ];

        LD_LIBRARY_PATH = with pkgs;
            lib.makeLibraryPath
            ([ pkg-config stdenv.cc.cc.lib libffi ncurses zlib libuv]);

        #MUSL_LD_LIBRARY_PATH = with pkgs;
            #lib.makeLibraryPath
            #([ pkg-config stdenv.cc.cc.lib libffi ncurses zlib libuv musl]);

        shellHook = ''
          ${pkgs.rustToolchain}/bin/cargo --version
          export LLVM_SYS_130_PREFIX=${pkgs.llvmPackages_13.llvm.dev};
          export RUST_BACKTRACE=1
          export RUST_FLAGS="-A dead_code"

        '';
      };
    });
}
