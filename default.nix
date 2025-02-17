{ mkDerivation, base, containers, data-default, data-fix, dhall
, directory, filepath, generic-lens, hspec, lens, lib, mtl, text
, time, unix, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-masser";
  version = "0.1.0.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base containers data-default data-fix dhall directory filepath
    generic-lens lens mtl text time unix xmonad xmonad-contrib
  ];
  testHaskellDepends = [ base dhall hspec ];
  license = lib.licenses.bsd3;
}
