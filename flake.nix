{
  description = "inCode";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    purifix.url = "github:purifix/purifix";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, purifix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          purifix.overlay
          haskellNix.overlay
          (final: prev: {
            inCode = final.haskell-nix.project' {
              name = "inCode";
              src = ./.;
              compiler-nix-name = "ghc963";
              shell = {
                withHoogle = false;
                tools = {
                  cabal = { };
                  hlint = { };
                  haskell-language-server = { };
                };
              };
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        inCode-purescript = pkgs.purifix { src = ./app-purescript; };
        flake = pkgs.inCode.flake { };
      in
      flake
        //
      {
        packages.default = flake.packages."inCode:exe:inCode-build";
        packages.inCode-purescript = inCode-purescript;
      }
    );
}

