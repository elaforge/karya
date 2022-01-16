{ mkDerivation, base, ghc-prim, lib, unix }:
mkDerivation {
  pname = "base-compat";
  version = "0.12.1";
  sha256 = "fb683cb4041b88cab1d0849f70ebd26b342c734a9ef6f75233c1602d53a015fd";
  libraryHaskellDepends = [ base ghc-prim unix ];
  description = "A compatibility layer for base";
  license = lib.licenses.mit;
}
