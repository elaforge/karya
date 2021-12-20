{ mkDerivation, base, bytestring, deepseq, ghc-prim, HUnit
, integer-gmp, QuickCheck, random, lib, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, unix
}:
mkDerivation {
  pname = "hashable";
  version = "1.3.1.0";
  sha256 = "8061823a4ac521b53912edcba36b956f3159cb885b07ec119af295a6568ca7c4";
  libraryHaskellDepends = [
    base bytestring deepseq ghc-prim integer-gmp text
  ];
  testHaskellDepends = [
    base bytestring ghc-prim HUnit QuickCheck random test-framework
    test-framework-hunit test-framework-quickcheck2 text unix
  ];
  homepage = "http://github.com/haskell-unordered-containers/hashable";
  description = "A class for types that can be converted to a hash value";
  license = lib.licenses.bsd3;
}
