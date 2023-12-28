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
        haskellFlake = pkgs.inCode.flake { };
        inCode = rec {
          purescript = pkgs.purifix { src = ./app-purescript; };
          haskell = haskellFlake.packages."inCode:exe:inCode-build";
          web = pkgs.stdenv.mkDerivation {
            impure = true;
            name = "inCode";
            buildInputs = [ purescript haskell ];
            src = pkgs.nix-gitignore.gitignoreSourcePure ''
              *
              !/code-samples/
              !/config/
              !/copy/
              !/css/
              !/js/
              !/latex/
              !/scss/
              !/static/
            '' ./.;
            LANG = "en_US.UTF-8";
            LOCALE_ARCHIVE = pkgs.lib.optionalString
              (pkgs.buildPlatform.libc == "glibc")
              "${pkgs.glibcLocales}/lib/locale/locale-archive";
            preBuild = ''
              mkdir _purescript
              find ${purescript}/output -type f -name index.js -exec sh -c '
                file={};
                subdir=$(basename $(dirname $file))
                cp $file _purescript/$(echo $subdir | tr "[:upper:]" "[:lower:]").js' \;
            '';
            buildPhase = ''
              export XDG_CACHE_HOME=$(mktemp -d)
              ${haskell}/bin/inCode-build build --verbose
            '';
            installPhase = ''
              mkdir -p "$out/dist"
              cp -a _site/. "$out/dist"
            '';
          };
        };
      in
      haskellFlake
      //
      {
        packages.default = inCode.web;
        packages.inCode = inCode;
      }
    );
}

