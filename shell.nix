{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let
  easy-dhall-nix-src = with builtins;
    fetchgit { inherit (fromJSON (readFile ./easy-dhall-nix.json)) url rev sha256 fetchSubmodules; };
  easy-dhall-nix = import easy-dhall-nix-src {};
  hp = haskellPackages.extend (self: super: {
    xmonad-masser = self.callPackage ./. {};
  });
  easy-hls-src = fetchFromGitHub {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    inherit (builtins.fromJSON (builtins.readFile ./easy-hls-nix.json)) rev sha256;
  };
  easy-hls = callPackage easy-hls-src { ghcVersions = [ hp.ghc.version ]; };

in

hp.shellFor {
  packages = h: [h.xmonad-masser];
  withHoogle = true;
  buildInputs = [
    entr
    cabal-install
    hp.hlint
    stylish-haskell
    ghcid
    cabal2nix
    easy-hls
    easy-dhall-nix.dhall-lsp-simple
    easy-dhall-nix.dhall-simple
  ];
}
