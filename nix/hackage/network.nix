{ mkDerivation, base, bytestring, deepseq, directory, hspec
, hspec-discover, HUnit, QuickCheck, stdenv, temporary
}:
mkDerivation {
  pname = "network";
  version = "3.1.2.1";
  sha256 = "fcaa954445cb575ff04d088e719452e356324b6acb98c5aefd2541a069439d4a";
  revision = "1";
  editedCabalFile = "12swsygnsnyvfjm3p9b6z8jh8vbw3q5akmggacsvr4fdm2p6v38q";
  libraryHaskellDepends = [ base bytestring deepseq directory ];
  testHaskellDepends = [
    base bytestring directory hspec HUnit QuickCheck temporary
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell/network";
  description = "Low-level networking interface";
  license = stdenv.lib.licenses.bsd3;
}
