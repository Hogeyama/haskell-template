{ pkgs
, compiler
}:
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
  haskPkgs = pkgs.haskell.packages.${compiler};
  drv = haskPkgs.callCabal2nix "my-template" src { };
in
drv
