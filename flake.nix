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
            (pkgs.haskell-language-server.override {
              supportedGhcVersions = [ "9103" ];
            })
            haskellPackages.stack
            imagemagick
            lapack
            libllvm
          ];
        };
        formatter = pkgs.nixfmt-tree; # Format this file with `nix fmt`
      }
    );
}
