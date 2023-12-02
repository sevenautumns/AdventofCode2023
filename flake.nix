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
          capital = text: (lib.strings.toUpper (builtins.substring 0 1 text) + builtins.substring 1 100 text);
          builder = name: pkgs.runCommand name { } ''
            mkdir --parent $out/bin
            ${pkgs.haskellPackages.ghc}/bin/ghc -odir /build -hidir /build -o $out/bin/${name} ${self}/${name}.hs
          '';
          package_names = map (x: "day" + (toString x)) (lib.range 1 days);
          days = 2;
        in
        rec {
          packages = lib.genAttrs package_names (name: builder name);

          devShells.default = (pkgs.devshell.mkShell {
            imports = [ "${devshell}/extra/git/hooks.nix" ];
            name = "Advent of Code (Haskell)";
            packages = with pkgs; [
              haskellPackages.ghc
              haskell-language-server
            ];
            commands = (builtins.map
              (name: {
                name = "run-${name}";
                command = "${packages.${name}}/bin/${name}";
                help = "Run ${capital name} code";
              })
              package_names)
            ++ (builtins.map
              (name: {
                name = "watch-${name}";
                command = "${pkgs.cargo-watch}/bin/cargo-watch watch --watch $PRJ_ROOT/${name}.hs --workdir $PRJ_ROOT --shell \"run-${name}\"";
                help = "Watch ${capital name}";
                category = "dev";
              })
              package_names)
            ;
          });

          # always check these
          checks = {
            nixpkgs-fmt = pkgs.runCommand "nixpkgs-fmt"
              {
                nativeBuildInputs = [ pkgs.nixpkgs-fmt ];
              } "nixpkgs-fmt --check ${./.}; touch $out";
          };

          # instructions for the CI server
          hydraJobs = (nixpkgs.lib.filterAttrs (n: _: n != "default") packages)
            // checks;
        });
}

