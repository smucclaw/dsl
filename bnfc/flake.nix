{
  description = "A very basic flake";

  # inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-20.09;
  inputs.gf-nix.url = github:anka-213/cclaw-nix-stuff/nix-flakes;
  inputs.gf-nix.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, gf-nix }: {

    # defaultPackage.x86_64-darwin = throw nixpkgs;
    # defaultPackage.x86_64-darwin = null;
    # packages.x86_64-darwin.hello = nixpkgs.legacyPackages.x86_64-darwin.hello;
    # defaultPackage.x86_64-darwin = self.packages.x86_64-darwin.hello;
    # packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
    # defaultPackage.x86_64-linux = self.packages.x86_64-linux.hello;
    devShell = builtins.mapAttrs
      (arch: pkgs:
        let
          gf-pkgs = gf-nix.packages.${arch};
          # inherit (gf-pkgs) gf-wordnet;
        in
        pkgs.mkShell {
          buildInputs = [
            pkgs.emacs
            pkgs.stack
            pkgs.nix
            gf-pkgs.bnfc
            gf-pkgs.gf-with-rgl
            pkgs.haskellPackages.happy
            pkgs.haskellPackages.alex
            pkgs.graphviz
          ];
          # inputsFrom = throw nixpkgs;
          shellHook = ''
            export NIX_PATH=nixpkgs=${nixpkgs};
            export GF_LIB_PATH=$GF_LIB_PATH''${GF_LIB_PATH:+':'}${gf-pkgs.gf-wordnet}
          '';
          # inputsFrom = [ self.packages.${arch}.lambda-launcher-unwrapped self.packages.${arch}.lambda-launcher ];
          # inputsFrom = [ self.packages.${arch}.lambda-launcher ];
          # buildInputs = [ self.packages.${arch}.haskell-language-server ];
          # buildInputs = [ self.packages.${arch}.ghc-wrapper ];
          # shellHook = "eval $(egrep ^export ${self.packages.${arch}.ghc-wrapper}/bin/ghc)";
          # shellHook = "eval $(egrep ^export ${pkgs.ghc-wrapper}/bin/ghc)";
        })
      nixpkgs.legacyPackages;
  };
}
