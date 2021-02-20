{ mkDerivation, base, bytestring, deepseq, directory, hspec
, hspec-discover, HUnit, stdenv
}:
mkDerivation {
  pname = "network";
  version = "3.1.1.1";
  sha256 = "d7ef590173fff2ab522fbc167f3fafb867e4ecfca279eb3ef0d137b51f142c9a";
  libraryHaskellDepends = [ base bytestring deepseq ];
  testHaskellDepends = [ base bytestring directory hspec HUnit ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell/network";
  description = "Low-level networking interface";
  license = stdenv.lib.licenses.bsd3;
}
