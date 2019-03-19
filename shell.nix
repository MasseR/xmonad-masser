{ nixpkgs ? import <nixpkgs> {} }:

(nixpkgs.callPackage ./release.nix {}).shell
