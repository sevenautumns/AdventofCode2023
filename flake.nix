{
  description = "Advent of Code (Haskell)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "git+https://github.com/numtide/flake-utils.git";
    devshell.url = "github:numtide/devshell";
  };
  outputs = { self, nixpkgs, utils, devshell, ... }@inputs:
    utils.lib.eachSystem [ "x86_64-linux" "i686-linux" "aarch64-linux" ]
      (system:
        let
          lib = nixpkgs.lib;
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ devshell.overlays.default ];
          };
          basename = x: builtins.head (lib.strings.splitString "-" x);
          capital = text: (lib.strings.toUpper (builtins.substring 0 1 text) + builtins.substring 1 100 text);
          builder = name: pkgs.runCommand name { } ''
            mkdir --parent $out/bin
            ${pkgs.haskellPackages.ghc}/bin/ghc -O2 -odir /build -hidir /build -o $out/bin/${name} ${self}/${basename name}/${name}.hs
          '';
          package_names = (map (x: "day" + (toString x)) (lib.range 1 days)) ++ (map (x: "day" + (toString x) + "-part2") (lib.range 1 days));
          days = 12;
        in
        rec {
          packages = lib.genAttrs package_names (name: builder name);

          devShells.default = (pkgs.devshell.mkShell {
            imports = [ "${devshell}/extra/git/hooks.nix" ];
            name = "Advent of Code (Haskell)";
            packages = with pkgs; [
              haskellPackages.ghc
              haskell-language-server
              ormolu
              cocogitto
            ];
            git.hooks = {
              enable = true;
              pre-commit.text = "nix flake check";
            };
            commands = (builtins.map
              (name:
                {
                  name = "run-${name}";
                  command = ''
                    mkdir --parent $PRJ_ROOT/${basename name}
                    cd $PRJ_ROOT/${basename name}
                    nix run .#${name}
                  '';
                  help = "Run ${capital name} code";
                })
              package_names)
            ++ (builtins.map
              (name:
                {
                  name = "watch-${name}";
                  command = ''
                    mkdir --parent $PRJ_ROOT/build
                    mkdir --parent $PRJ_ROOT/${basename name}
                    ${pkgs.cargo-watch}/bin/cargo-watch watch \
                      --workdir $PRJ_ROOT/${basename name} \
                      --shell "${pkgs.ghc}/bin/ghc -O2 -odir $PRJ_ROOT/build -hidir $PRJ_ROOT/build -o $PRJ_ROOT/build/${name} $PRJ_ROOT/${basename name}/${name}.hs && $PRJ_ROOT/build/${name}"
                  '';
                  help = "Watch ${capital name}";
                  category = "dev";
                })
              package_names) ++ [{
              name = "format-haskell";
              command = ''
                cd $PRJ_ROOT
                ${pkgs.ormolu}/bin/ormolu --mode inplace */*.hs
              '';
              help = "format all haskell files";
              category = "dev";
            }]
            ;
          });

          # always check these
          checks = {
            nixpkgs-fmt = pkgs.runCommand "nixpkgs-fmt"
              { } "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt --check ${self}; touch $out";
            ormolu = pkgs.runCommand "ormolu"
              { } "${pkgs.ormolu}/bin/ormolu --mode check ${self}/*/*.hs; touch $out";
            build-all = pkgs.runCommand "build-all"
              {
                nativeBuildInputs = map (x: self.packages.${system}.${x}) package_names;
              } "touch $out";
          };

          # instructions for the CI server
          hydraJobs = (nixpkgs.lib.filterAttrs (n: _: n != "default") packages)
            // checks;
        });
}

