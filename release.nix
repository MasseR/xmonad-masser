{ nixpkgs ? import <nixpkgs> {} }:

rec {
  xmonad-masser = nixpkgs.haskellPackages.callPackage ./default.nix {};
  shell = nixpkgs.buildEnv {
    name = "shell";
    paths = [];
    buildInputs = with nixpkgs.haskellPackages; [
      (ghcWithPackages (_: xmonad-masser.buildInputs ++ xmonad-masser.propagatedBuildInputs))
      ghcid
      cabal-install
      nixpkgs.pkgs.binutils
      stylish-haskell
    ];
  };
}
