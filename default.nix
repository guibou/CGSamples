{ pkgs ? import <nixpkgs> {} }:
with pkgs.haskell.packages.ghc822;
developPackage {
  root = ./.;
  overrides = self: super: {
  };
}
