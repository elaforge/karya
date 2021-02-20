{ mkDerivation, array, base, bytestring, containers, ghc-prim
, QuickCheck, stdenv, test-framework, test-framework-quickcheck2
}:
mkDerivation {
  pname = "cereal";
  version = "0.5.8.1";
  sha256 = "2d9e88ac934b9ebc058097c72011ff59f3f146176310e1c957a0e4cf63681bd7";
  libraryHaskellDepends = [
    array base bytestring containers ghc-prim
  ];
  testHaskellDepends = [
    base bytestring QuickCheck test-framework
    test-framework-quickcheck2
  ];
  homepage = "https://github.com/GaloisInc/cereal";
  description = "A binary serialization library";
  license = stdenv.lib.licenses.bsd3;
}
