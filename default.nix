{ lib, haskellPackages }:

let
  filtered = src: lib.sourceByRegex src [
    "^src.*"
    "Setup.hs"
    ".*cabal"
    "LICENSE"
    ];

in

haskellPackages.callCabal2nix "xmonad-masser" (filtered ./.) {}
