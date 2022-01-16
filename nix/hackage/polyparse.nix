{ mkDerivation, base, bytestring, lib, text }:
mkDerivation {
  pname = "polyparse";
  version = "1.13";
  sha256 = "1c4c72980e1e5a4f07fea65ca08b2399581d2a6aa21eb1078f7ad286c279707b";
  revision = "3";
  editedCabalFile = "0hdd8vfdsipqmz1c8snnihff5419hc181ky8lspk8aq7pyv7ic45";
  libraryHaskellDepends = [ base bytestring text ];
  doCheck = false;
  homepage = "http://code.haskell.org/~malcolm/polyparse/";
  description = "A variety of alternative parser combinator libraries";
  license = "LGPL";
}
