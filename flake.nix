{
  description = "A very basic flake";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-unstable"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils }:
  {
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override ( old: {
        overrides = final.lib.composeExtensions ( old.overrides or (_: _: {})) (f: p: {
          xmonad-masser = f.callPackage ./. {};
        });
      } );
    };
  }
    //
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] ( system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        hp = pkgs.haskellPackages;
      in
      rec {

        packages = { inherit (hp) xmonad-masser; };

        defaultPackage = packages.xmonad-masser;
        devShell = hp.shellFor {
          packages = h: [h.xmonad-masser];
          withHoogle = false;
          buildInputs = with pkgs; [
            entr
            cabal-install
            hp.hlint
            stylish-haskell
            ghcid
            hp.haskell-language-server

            sqlite-interactive
          ];
        };
      }
    );
}
