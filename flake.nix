{
  description = "inCode";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    purifix.url = "github:purifix/purifix";
    easy-purescript.url = "github:justinwoo/easy-purescript-nix";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, purifix, easy-purescript }:
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
              ${haskell}/bin/inCode-build build
            '';
            installPhase = ''
              mkdir -p "$out/dist"
              cp -a _site/. "$out/dist"
            '';
          };
        };
        rebuild-js =
          let
            buildSingleDep = name: value:
              let
                srcGlob = "purescript/${name}/src/**/*.purs";
                buildDir = "$HAKYLL_DIR/_purescript-build/${name}";
                mainModule = "${buildDir}/Main/index.js";
                outFile = "$HAKYLL_DIR/_purescript/${name}.js";
              in
              ''
                mkdir -p ${buildDir}
                purs compile ${toString value.globs} ${srcGlob} -o ${buildDir}
                chmod -R +w ${buildDir}
                echo "import {main} from '${mainModule}'; main()" | esbuild --bundle --outfile=${outFile} --format=iife
              '';
          in
          pkgs.writeShellScriptBin
            "rebuild-js"
            ''
              mkdir -p "$HAKYLL_DIR/_purescript";
              ${lib.concatStringsSep "\n" (lib.mapAttrsToList buildSingleDep inCode.purescript)}
            '';
        all-js-globs = lib.flatten (lib.mapAttrsToList (name: value: value.globs) inCode.purescript);
      in
      {
        packages = {
          inherit inCode;
          default = inCode.web;
        };
        devShells = {
          haskell-dev = haskellFlake.devShell;
          purescript-dev = lib.mapAttrs (name: value: value.develop) inCode.purescript;
          default = pkgs.mkShell {
            shellHook = ''
              export HAKYLL_DIR=$(mktemp -d)
              echo "Available commands: rebuild-js inCode-build"
              echo "Hakyll working directory: \$HAKYLL_DIR"

              export PURS_IDE_SOURCES='${toString all-js-globs}'
              mkdir -p purescript/output
              mkdir -p $HAKYLL_DIR/_purescript
              ${lib.concatStringsSep "\n"
              (lib.mapAttrsToList
                (name: value:
                    ''
                    cp -a ${value.deps}/output/. purescript/output
                    chmod -R +w purescript/output
                    cp ${value.bundle-app} $HAKYLL_DIR/_purescript/${name}.js
                    ''
                )
                inCode.purescript
              )}
              chmod -R +w $HAKYLL_DIR/_purescript

              for srcDir in code-samples config copy css js latex scss static; do
                ln -s "$PWD/$srcDir" $HAKYLL_DIR
              done
            '';
            nativeBuildInputs = [ pkgs.esbuild pkgs.purescript ]
              ++ haskellFlake.devShell.nativeBuildInputs
              ++ lib.attrValues inCode.purescript
              ++ map (value: value.develop.buildInputs) (lib.attrValues inCode.purescript)
              ++ [ easy-purescript.packages.${system}.purty easy-purescript.packages.${system}.spago ];
            packages = [
              rebuild-js
              inCode.haskell
            ];
          };
        };
      }
    );
}

