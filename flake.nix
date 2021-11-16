{
  description = "A very basic flake";

  inputs = {
    easy-hls-src = { url = "github:jkachmar/easy-hls-nix"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils, easy-hls-src }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] ( system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hp = pkgs.haskellPackages.extend (self: super: {
            xmonad-masser = self.callPackage ./. {};
          });
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
