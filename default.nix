{ mkDerivation, base, containers, data-default, directory, filepath
, generic-lens, lens, mtl, stdenv, text, time, unix, xmonad
, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-masser";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers data-default directory filepath generic-lens lens
    mtl text time unix xmonad xmonad-contrib
  ];
  license = stdenv.lib.licenses.bsd3;
}
