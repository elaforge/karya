{ mkDerivation, base, base-orphans, deepseq, ghc-prim, HUnit
, primitive, QuickCheck, random, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, transformers
}:
mkDerivation {
  pname = "vector";
  version = "0.12.3.0";
  sha256 = "15f818505adda63e7f484ecdf92dbb3c1ec76a9def004c9424db8fa6bc41b703";
  libraryHaskellDepends = [ base deepseq ghc-prim primitive ];
  testHaskellDepends = [
    base base-orphans HUnit primitive QuickCheck random tasty
    tasty-hunit tasty-quickcheck template-haskell transformers
  ];
  homepage = "https://github.com/haskell/vector";
  description = "Efficient Arrays";
  license = stdenv.lib.licenses.bsd3;
}
