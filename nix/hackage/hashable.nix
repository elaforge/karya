{ mkDerivation, base, bytestring, containers, data-array-byte
, deepseq, filepath, ghc-bignum, ghc-prim, HUnit, lib, QuickCheck
, random, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, unix
}:
mkDerivation {
  pname = "hashable";
  version = "1.4.2.0";
  sha256 = "1b4000ea82b81f69d46d0af4152c10c6303873510738e24cfc4767760d30e3f8";
  revision = "1";
  editedCabalFile = "12nmnmm2kyjalkvmz0l1l895ikc938lwppx8iykxnhamblrr4msq";
  libraryHaskellDepends = [
    base bytestring containers data-array-byte deepseq filepath
    ghc-bignum ghc-prim text
  ];
  testHaskellDepends = [
    base bytestring ghc-prim HUnit QuickCheck random test-framework
    test-framework-hunit test-framework-quickcheck2 text unix
  ];
  doCheck = false;
  homepage = "http://github.com/haskell-unordered-containers/hashable";
  description = "A class for types that can be converted to a hash value";
  license = lib.licenses.bsd3;
}
