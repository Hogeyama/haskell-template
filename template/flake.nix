{
  description = "TODO";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };

  outputs = { nixpkgs, flake-utils, ... }:
    let
      compiler-version = "944";
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
              pkgs.lib.trivial.pipe (self.callCabal2nix "my-template" src { }) [
                pkgs.haskell.lib.justStaticExecutables
                pkgs.haskell.lib.doBenchmark
              ];
            my-shell = self.shellFor {
              withHoogle = true;
              packages = _: [ self.my-package ];
              doBenchmark = true;
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
            async = pkgs.haskell.lib.overrideCabal
              (self.callHackageDirect
                {
                  pkg = "async";
                  ver = "2.2.4";
                  sha256 = "sha256-pYBuzx0NRMcvZtxmMeKZSXwyVvTVoHy5LwfvTTf2XnI=";
                }
                { })
              (drv: {
                editedCabalFile = "sha256-RjZ9wMgybcvre5PyALVnSRwvYCm8z4Iri7Ju5mA5fgg=";
                revision = "3"; # allow base-4.18
              });
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
