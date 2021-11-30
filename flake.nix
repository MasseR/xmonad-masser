{
  description = "A very basic flake";

  inputs = {
    easy-hls = {
      url = "github:jkachmar/easy-hls-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils, easy-hls }:
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
        hls = easy-hls.defaultPackage.${system};
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
