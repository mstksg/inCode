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
        lib = nixpkgs.lib;
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
          purescript = pkgs.purifix { src = ./purescript; };
          haskell = haskellFlake.packages."inCode:exe:inCode-build";
          web = pkgs.stdenv.mkDerivation {
            name = "inCode";
            buildInputs = [ haskell ] ++ lib.attrValues purescript;
            srcs = [
              ./code-samples
              ./config
              ./copy
              ./css
              ./js
              ./latex
              ./scss
              ./static
            ];
            LANG = "en_US.UTF-8";
            LOCALE_ARCHIVE = pkgs.lib.optionalString
              (pkgs.buildPlatform.libc == "glibc")
              "${pkgs.glibcLocales}/lib/locale/locale-archive";
            unpackPhase = ''
              for srcFile in $srcs; do
                cp -a $srcFile/. $(stripHash $srcFile)
              done

              mkdir _purescript
              ${
                lib.concatStringsSep "\n" (lib.mapAttrsToList
                    (name: value: ''cp ${value.bundle-app} _purescript/${name}.js'')
                    purescript
                  )
               }
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
        build-js =
          let
            buildSingleDep = name: value:
              let
                srcGlob = "purescript/${name}/src/**/*.purs";
                buildDir = "_purescript-build/${name}";
                mainModule = "./${buildDir}/Main/index.js";
                outFile = "_purescript/${name}.js";
              in
              ''
                mkdir -p ${buildDir}
                purs compile ${toString value.globs} ${srcGlob} -o ${buildDir}
                chmod -R +w ${buildDir}
                echo "import {main} from '${mainModule}'; main()" | esbuild --bundle --outfile=${outFile} --format=iife
              '';
          in
          pkgs.writeShellScriptBin
            "build-js"
            ''
              mkdir -p _purescript;
              ${lib.concatStringsSep "\n" (lib.mapAttrsToList buildSingleDep inCode.purescript)}
            '';
      in
      {
        packages = {
          inherit inCode;
          default = inCode.web;
        };
        devShells = {
          haskell-dev = pkgs.mkShell {
            nativeBuildInputs = haskellFlake.devShell.nativeBuildInputs;
          };
          default = pkgs.mkShell {
            shellHook = ''
              echo "Available commands: build-js inCode-build clean-all"
            '';
            nativeBuildInputs = [ pkgs.esbuild pkgs.purescript ]
              ++ haskellFlake.devShell.nativeBuildInputs
              ++ lib.mapAttrsToList (name: value: value.develop.buildInputs) inCode.purescript;
            packages = [
              build-js
              inCode.haskell
              (pkgs.writeShellScriptBin "clean-all" ''
                rm -r _purescript
                rm -r _purescript-build
                inCode-build clean
              '')
            ];
          };
        };
      }
    );
}

