{ mkDerivation, async, base, constraints, deepseq, HUnit
, lifted-base, monad-control, mtl, lib, tasty, tasty-bench
, tasty-expected-failure, tasty-hunit, tasty-th, transformers-base
}:
mkDerivation {
  pname = "lifted-async";
  version = "0.10.2";
  sha256 = "612893b906840cd1e4a9dd12c6466244ada8f8c8ab604f69d6c491db77de902a";
  libraryHaskellDepends = [
    async base constraints lifted-base monad-control transformers-base
  ];
  testHaskellDepends = [
    async base HUnit lifted-base monad-control mtl tasty
    tasty-expected-failure tasty-hunit tasty-th
  ];
  benchmarkHaskellDepends = [ async base deepseq tasty-bench ];
  homepage = "https://github.com/maoe/lifted-async";
  description = "Run lifted IO operations asynchronously and wait for their results";
  license = lib.licenses.bsd3;
}
