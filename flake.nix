{
  description = "TODO";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils/master";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      compiler = "ghc921";
      supportedSystems = [ "x86_64-linux" ];

      outputs-overlay = pkgs: prev: {
        my-package = import ./. { inherit pkgs compiler; };
        my-shell = import ./shell.nix { inherit pkgs compiler; };
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
        defaultPackage = pkgs.my-package;
        devShell = pkgs.my-shell;
      }
    );
}
