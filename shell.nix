{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let pkg = haskellPackages.callPackage ./. {};

in

mkShell {

  buildInputs = [
    ghcid
    hlint
    stylish-haskell
    haskellPackages.cabal-install
    cabal2nix
    (haskellPackages.ghcWithPackages (_: pkg.buildInputs ++ pkg.propagatedBuildInputs))
  ];
}
