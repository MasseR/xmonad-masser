{
  description = "A very basic flake";

  inputs = {
    easy-hls-src = { url = "github:jkachmar/easy-hls-nix"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils, easy-hls-src }:
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
        easy-hls = pkgs.callPackage easy-hls-src { ghcVersions = [ hp.ghc.version ]; };
      in
      rec {

        packages = { inherit (hp) xmonad-masser; };

        defaultPackage = packages.xmonad-masser;
        devShell = hp.shellFor {
          packages = h: [h.xmonad-masser];
          withHoogle = true;
          buildInputs = with pkgs; [
            entr
            cabal-install
            hp.hlint
            stylish-haskell
            ghcid
            easy-hls

            sqlite-interactive

            hp.graphmod
          ];
        };
      }
    );
}
