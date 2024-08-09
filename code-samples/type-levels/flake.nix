{
  description = "levels of type safety code samples";
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
              (haskell.lib.unmarkBroken typelits-witnesses)
              # finite-typelits
              # ghc-typelits-natnormalise
              # linear
              # mwc-random
              # vector-sized
            ]))
          ];
        };
      }
    );
}

