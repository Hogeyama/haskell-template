{
  description = "TODO";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils/master";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };

  outputs = { nixpkgs, flake-utils, ... }:
    let
      compiler-version = "924";
      supportedSystems = [ "x86_64-linux" ];

      outputs-overlay = pkgs: prev: {
        haskPkgs = pkgs.haskell.packages."ghc${compiler-version}".override {
          overrides = self: super: {
            my-package =
              let
                src = pkgs.lib.sourceByRegex ./. [
                  "app"
                  "app/.*"
                  "lib"
                  "lib/.*"
                  "test"
                  "test/.*"
                  "benchmark"
                  "benchmark/.*"
                  "Setup.hs"
                  "my-template.cabal"
                  "README.md"
                  "CHANGELOG.md"
                  "LICENSE"
                ];
              in
              pkgs.haskellPackages.callCabal2nix "my-template" src { };
            my-shell = self.shellFor {
              withHoogle = true;
              packages = _: [ self.my-package ];
              buildInputs = with pkgs; [
                nixfmt
                cabal-install
                haskellPackages.cabal-fmt
                haskellPackages.fourmolu
                haskellPackages.hlint
                (haskell-language-server.override {
                  supportedGhcVersions = [ compiler-version ];
                })
              ];
            };
          };
        };
      };
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ outputs-overlay ];
        };
      in
      {
        packages.default = pkgs.haskPkgs.my-package;
        devShells.default = pkgs.haskPkgs.my-shell;
      }
    );
}
