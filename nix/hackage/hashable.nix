{ mkDerivation, base, base-orphans, bytestring, containers, deepseq
, ghc-prim, HUnit, integer-gmp, lib, QuickCheck, random
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, unix
}:
mkDerivation {
  pname = "hashable";
  version = "1.4.0.2";
  sha256 = "83606edd356d914c075ecd44f6d5fe91a3b186aa0683c8dd8c9a7e8e22a47600";
  libraryHaskellDepends = [
    base base-orphans bytestring containers deepseq ghc-prim
    integer-gmp text
  ];
  testHaskellDepends = [
    base bytestring ghc-prim HUnit QuickCheck random test-framework
    test-framework-hunit test-framework-quickcheck2 text unix
  ];
  homepage = "http://github.com/haskell-unordered-containers/hashable";
  description = "A class for types that can be converted to a hash value";
  license = lib.licenses.bsd3;
}
