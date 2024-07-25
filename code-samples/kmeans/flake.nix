{
  description = "inCode";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            (haskell.packages.ghc981.ghcWithPackages (p: with p; [
              finite-typelits
              linear
              statistics
              strict-tuple
              vector-sized
            ]))
          ];
        };
      }
    );
}
