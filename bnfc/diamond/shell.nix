# shell.nix
let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.alex
    pkgs.haskellPackages.happy
  ];
}