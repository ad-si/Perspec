{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      utils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            bash
            blas
            coreutils
            gnumake
            haskell.compiler.ghc910
            haskellPackages.cabal-fmt
            haskellPackages.cabal-install
            haskellPackages.fourmolu
            haskellPackages.hpack # Generates `perspec.cabal` for `cabal build`
            (pkgs.haskell-language-server.override {
              supportedGhcVersions = [ "9103" ];
            })
            haskellPackages.stack
            lapack
            libllvm
            libwebp
            nodejs # Provides `npx` for `markdown-toc` in `make format`
            pkg-config
            zlib
          ]
          # Native libraries needed by GLFW and OpenGL on Linux
          ++ lib.optionals stdenv.isLinux [
            libGL
            libGLU
            libx11
            libxcursor
            libxext
            libxi
            libxinerama
            libxrandr
            libxxf86vm
          ];
        };
        formatter = pkgs.nixfmt-tree; # Format this file with `nix fmt`
      }
    );
}
