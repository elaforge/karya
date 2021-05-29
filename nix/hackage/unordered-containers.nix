{ mkDerivation, base, bytestring, ChasingBottoms, containers
, deepseq, gauge, hashable, hashmap, HUnit, mtl, QuickCheck, random
, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "unordered-containers";
  version = "0.2.13.0";
  sha256 = "86b01369ab8eb311383a052d389337e2cd71a63088323f02932754df4aa37b55";
  libraryHaskellDepends = [ base deepseq hashable ];
  testHaskellDepends = [
    base ChasingBottoms containers hashable HUnit QuickCheck random
    test-framework test-framework-hunit test-framework-quickcheck2
  ];
  benchmarkHaskellDepends = [
    base bytestring containers deepseq gauge hashable hashmap mtl
    random
  ];
  homepage = "https://github.com/haskell-unordered-containers/unordered-containers";
  description = "Efficient hashing-based container types";
  license = stdenv.lib.licenses.bsd3;
}
