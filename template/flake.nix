{
  description = "Sample Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/b756c485aacb5d87bfcd8d780d28816d41aa32ca";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    nix-bundle-elf.url = "github:Hogeyama/nix-bundle-elf/main";
    nix-bundle-elf.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    devshell.url = "github:numtide/devshell";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    let
      compiler-version = "946";
      outputs-overlay = pkgs: prev: rec {
        haskellPackages = pkgs.haskell.packages."ghc${compiler-version}".override {
          overrides = self: super: {
            # Add dependencies here if necessary. For example:
            scotty = self.callHackageDirect
              {
                pkg = "scotty";
                ver = "0.20.1";
                sha256 = "sha256-IqF51ZjZ1beYJApxAjAXBMlzGDvce6k2wRqzuTNK2OM=";
              }
              { };
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
              "app(/.*)?"
              "lib(/.*)?"
              "test(/.*)?"
              "benchmark(/.*)?"
              "Setup.hs"
              ".*\\.cabal"
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
            arion
            cabal-install
            haskellPackages.cabal-fmt
            haskellPackages.fourmolu
            haskellPackages.hlint
            haskellPackages.weeder
            (haskell-language-server.override {
              supportedGhcVersions = [ compiler-version ];
            })
          ];
          withHoogle = true;
          doBenchmark = true;
        };
      };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.flake-root.flakeModule
        inputs.devshell.flakeModule
        inputs.process-compose-flake.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-linux" ];
      perSystem = { config, lib, self', inputs', pkgs, system, ... }:
        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ outputs-overlay ];
          };

          packages = {
            default = pkgs.my-sample;
            bundled-exe = inputs.nix-bundle-elf.lib.${system}.single-exe {
              inherit pkgs;
              name = "my-sample-bundled";
              target = "${pkgs.my-sample}/bin/my-sample";
            };
            bundled-aws-lambda = inputs.nix-bundle-elf.lib.${system}.aws-lambda-zip {
              inherit pkgs;
              name = "my-sample-bundled";
              target = "${pkgs.my-sample}/bin/my-sample";
            };
            process-compose = pkgs.writeShellApplication {
              name = "process-compose";
              text = ''
                exec ${lib.getExe self'.packages.processes} -n default "$@"
              '';
            };
          };

          devshells.default = {
            devshell.motd = "";
            packagesFrom = [
              pkgs.shell-for-my-sample
            ];
          };

          checks = {
            integration-test = pkgs.runCommand "integration-test" { } ''
              ${lib.getExe self'.packages.process-compose} \
                -t=false \
                -n=dev \
                -n=test
              mkdir -p $out
            '';
          };

          process-compose."processes" =
            let
              pg_port = 5432;
              get_pgdata = pkgs.writeShellApplication {
                name = "get_pgdata";
                runtimeInputs = [ pkgs.postgresql ];
                text = ''
                  ROOT=$(${lib.getExe config.flake-root.package} 2>/dev/null || true)
                  ROOT=''${ROOT:-"$PWD"}
                  PGDATA=$ROOT/pgdata
                  echo "$PGDATA"
                '';
              };
            in
            {
              port = 12345;
              settings = {
                processes = {
                  server = {
                    namespace = "dev";
                    command = ''
                      ${lib.getExe pkgs.my-sample}
                    '';
                    readiness_probe = {
                      period_seconds = 3;
                      http_get = {
                        host = "localhost";
                        port = 3000;
                        path = "/healthcheck";
                      };
                    };
                    depends_on."postgres".condition = "process_healthy";
                  };
                  postgres = {
                    namespace = "default";
                    command = pkgs.writeShellApplication {
                      name = "postgres";
                      runtimeInputs = [ pkgs.postgresql ];
                      text = ''
                        set -e
                        PGDATA=$(${lib.getExe get_pgdata})
                        if ! [[ -e "$PGDATA/PG_VERSION" ]]; then
                            mkdir -p "$PGDATA"
                            initdb -U postgres -D "$PGDATA" -A trust
                        fi
                        postgres -D "$PGDATA" -k "$PGDATA" -p ${toString pg_port}
                      '';
                    };
                    readiness_probe = {
                      period_seconds = 1;
                      exec = {
                        command = "${lib.getExe (pkgs.writeShellApplication {
                          name = "pg_isready";
                          runtimeInputs = [ pkgs.postgresql ];
                          text = ''
                            PGDATA=$(${lib.getExe get_pgdata})
                            pg_isready --host "$PGDATA" -U postgres
                          '';
                        })}";
                      };
                    };
                  };
                  # testというkeyで作ると自動でflake checkが作成されるが、
                  # 現状カスタマイズ性が低いので別名で作って自前のcheckを用意することにする。
                  integration-test = {
                    namespace = "test";
                    command = pkgs.writeShellApplication {
                      name = "test";
                      runtimeInputs = [ pkgs.curl pkgs.jq ];
                      text = ''
                        ${lib.getExe pkgs.bash} ${./test/integration/test.bash}
                      '';
                    };
                    availability.exit_on_end = true;
                    depends_on."server".condition = "process_healthy";
                  };
                };
              };
            };

          legacyPackages = pkgs;
        };
    };
}
