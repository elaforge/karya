{ mkDerivation, array, base, bytestring, case-insensitive
, containers, criterion, deepseq, directory, filepath, ghc-prim
, http-types, parsec, QuickCheck, quickcheck-unicode, scientific
, stdenv, tasty, tasty-quickcheck, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "attoparsec";
  version = "0.13.2.3";
  sha256 = "3d1ac6713505e520ebb51f26cfb5f6e5a0825e25394a51419c7e035b60b2f2d9";
  revision = "2";
  editedCabalFile = "07wq7516m4ldfdk557pc52ap0ghjpcw6q23c3xmzlwwr3dgb1w86";
  libraryHaskellDepends = [
    array base bytestring containers deepseq scientific text
    transformers
  ];
  testHaskellDepends = [
    array base bytestring deepseq QuickCheck quickcheck-unicode
    scientific tasty tasty-quickcheck text transformers vector
  ];
  benchmarkHaskellDepends = [
    array base bytestring case-insensitive containers criterion deepseq
    directory filepath ghc-prim http-types parsec scientific text
    transformers unordered-containers vector
  ];
  homepage = "https://github.com/bos/attoparsec";
  description = "Fast combinator parsing for bytestrings and text";
  license = stdenv.lib.licenses.bsd3;
}
