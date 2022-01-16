{ mkDerivation, base, binary, deepseq, ghc-prim, hashable, hspec
, hspec-discover, lib, mtl, transformers, transformers-compat
, type-equality
}:
mkDerivation {
  pname = "constraints";
  version = "0.13.2";
  sha256 = "e0c8bc35294653f38c92ca511c1970be5b59c017bb17cb2d82d9b52b0794d1fd";
  revision = "1";
  editedCabalFile = "1h46cskb4ci2jigqlrd9x87n1i1d6q6w4r0in6i47ywmhjdv7xwg";
  libraryHaskellDepends = [
    base binary deepseq ghc-prim hashable mtl transformers
    transformers-compat type-equality
  ];
  testHaskellDepends = [ base hspec ];
  testToolDepends = [ hspec-discover ];
  doCheck = false;
  homepage = "http://github.com/ekmett/constraints/";
  description = "Constraint manipulation";
  license = lib.licenses.bsd2;
}
