{
  description = "A very basic flake";

  # inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-20.09;

  outputs = { self, nixpkgs }: {

    # defaultPackage.x86_64-darwin = throw nixpkgs;
    # defaultPackage.x86_64-darwin = null;
    # packages.x86_64-darwin.hello = nixpkgs.legacyPackages.x86_64-darwin.hello;
    # defaultPackage.x86_64-darwin = self.packages.x86_64-darwin.hello;
    # packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
    # defaultPackage.x86_64-linux = self.packages.x86_64-linux.hello;
    devShell = builtins.mapAttrs (arch: pkgs: pkgs.mkShell {
      buildInputs = [
          pkgs.emacs
          pkgs.stack
          pkgs.nix
          pkgs.haskellPackages.BNFC
          pkgs.haskellPackages.happy
          pkgs.graphviz
          ];
      # inputsFrom = throw nixpkgs;
      # shellHook = ''
      #     export NIX_PATH=nixpkgs=${nixpkgs};
      # '';
      # inputsFrom = [ self.packages.${arch}.lambda-launcher-unwrapped self.packages.${arch}.lambda-launcher ];
      # inputsFrom = [ self.packages.${arch}.lambda-launcher ];
      # buildInputs = [ self.packages.${arch}.haskell-language-server ];
      # buildInputs = [ self.packages.${arch}.ghc-wrapper ];
      # shellHook = "eval $(egrep ^export ${self.packages.${arch}.ghc-wrapper}/bin/ghc)";
      # shellHook = "eval $(egrep ^export ${pkgs.ghc-wrapper}/bin/ghc)";
    }) nixpkgs.legacyPackages;
  };
}
