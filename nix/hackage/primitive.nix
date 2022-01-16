{ mkDerivation, base, base-orphans, deepseq, ghc-prim, lib
, QuickCheck, quickcheck-classes-base, tagged, tasty, tasty-bench
, tasty-quickcheck, transformers, transformers-compat
}:
mkDerivation {
  pname = "primitive";
  version = "0.7.3.0";
  sha256 = "3c0cfda67f1ee6f7f65108ad6f973b5bbb35ddba34b3c87746a7448f787501dc";
  libraryHaskellDepends = [ base deepseq transformers ];
  testHaskellDepends = [
    base base-orphans ghc-prim QuickCheck quickcheck-classes-base
    tagged tasty tasty-quickcheck transformers transformers-compat
  ];
  benchmarkHaskellDepends = [
    base deepseq tasty-bench transformers
  ];
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = lib.licenses.bsd3;
}
