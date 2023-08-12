{
  description = "Sample Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/main";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    nix-bundle-elf.url = "github:Hogeyama/nix-bundle-elf/main";
    nix-bundle-elf.inputs.nixpkgs.follows = "nixpkgs";
    nix-bundle-elf.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { nixpkgs, flake-utils, nix-bundle-elf, ... }:
    let
      compiler-version = "944";
      supportedSystems = [ "x86_64-linux" ];

      outputs-overlay = pkgs: prev: rec {
        haskellPackages = pkgs.haskell.packages."ghc${compiler-version}".override {
          overrides = self: super: {
            # Add dependencies here if necessary. For example:
            # async = pkgs.haskell.lib.overrideCabal
            #   (self.callHackageDirect
            #     {
            #       pkg = "async";
            #       ver = "2.2.4";
            #       sha256 = "sha256-pYBuzx0NRMcvZtxmMeKZSXwyVvTVoHy5LwfvTTf2XnI=";
            #     }
            #     { })
            #   (drv: {
            #     editedCabalFile = "sha256-RjZ9wMgybcvre5PyALVnSRwvYCm8z4Iri7Ju5mA5fgg=";
            #     revision = "3";
            #   });
          };
        };
        my-sample =
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
              ".*.cabal"
              "README.md"
              "CHANGELOG.md"
              "LICENSE"
            ];
          in
          pkgs.lib.trivial.pipe
            (haskellPackages.callCabal2nix "my-sample" src { })
            [
              pkgs.haskell.lib.justStaticExecutables
              pkgs.haskell.lib.doBenchmark
            ];
        shell-for-my-sample = haskellPackages.shellFor {
          packages = _: [ my-sample ];
          buildInputs = with pkgs; [
            cabal-install
            haskellPackages.cabal-fmt
            haskellPackages.fourmolu
            haskellPackages.hlint
            (haskell-language-server.override {
              supportedGhcVersions = [ compiler-version ];
            })
          ];
          withHoogle = true;
          doBenchmark = true;
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
        packages = {
          default = pkgs.my-sample;
          bundled-exe = nix-bundle-elf.lib.${system}.single-exe {
            inherit pkgs;
            name = "my-sample-bundled";
            target = "${pkgs.my-sample}/bin/my-sample";
          };
          bundled-aws-lambda = nix-bundle-elf.lib.${system}.aws-lambda-zip {
            inherit pkgs;
            name = "my-sample-bundled";
            target = "${pkgs.my-sample}/bin/my-sample";
          };
        };
        devShells = {
          default = pkgs.shell-for-my-sample;
        };
      }
    );
}
