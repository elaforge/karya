{ mkDerivation, base, containers, deepseq, process, random
, splitmix, stdenv, template-haskell, transformers
}:
mkDerivation {
  pname = "QuickCheck";
  version = "2.13.2";
  sha256 = "7b560baa5853de777702dc23a6f2126ae4adbfdab163295bc56323a706914610";
  revision = "1";
  editedCabalFile = "0ynhx1n135b0zg539c9m7gp75dykm93pqqlp5xz2w4kmpxjp4vk3";
  libraryHaskellDepends = [
    base containers deepseq random splitmix template-haskell
    transformers
  ];
  testHaskellDepends = [ base deepseq process ];
  homepage = "https://github.com/nick8325/quickcheck";
  description = "Automatic testing of Haskell programs";
  license = stdenv.lib.licenses.bsd3;
}
