{ mkDerivation, base, base-compat, ghc-prim, hspec, hspec-discover
, lib, OneTuple, QuickCheck
}:
mkDerivation {
  pname = "base-compat-batteries";
  version = "0.12.1";
  sha256 = "f98f3cdd8231edb4826744904d61f7672b9b628c7072c45684d3f7f1f55d838b";
  revision = "2";
  editedCabalFile = "0p522rvawkpyr12jwa0cikpqihvq2xd99nr95l0s3zdjf1srv0gf";
  libraryHaskellDepends = [ base base-compat ghc-prim OneTuple ];
  testHaskellDepends = [ base hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  description = "base-compat with extra batteries";
  license = lib.licenses.mit;
}
