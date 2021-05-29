{ mkDerivation, base, bytestring, criterion, deepseq, HUnit
, QuickCheck, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "base64-bytestring";
  version = "1.2.0.1";
  sha256 = "af09b17d072eb1391d91e30b4186dd1797330647ef79268ecd7fcce8f5afc638";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  benchmarkHaskellDepends = [ base bytestring criterion deepseq ];
  homepage = "https://github.com/haskell/base64-bytestring";
  description = "Fast base64 encoding and decoding for ByteStrings";
  license = stdenv.lib.licenses.bsd3;
}
