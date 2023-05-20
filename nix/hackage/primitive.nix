{ mkDerivation, base, base-orphans, data-array-byte, deepseq
, ghc-prim, lib, QuickCheck, quickcheck-classes-base, tagged, tasty
, tasty-bench, tasty-quickcheck, template-haskell, transformers
, transformers-compat
}:
mkDerivation {
  pname = "primitive";
  version = "0.8.0.0";
  sha256 = "5553c21b4a789f9b591eed69e598cc58484c274af29250e517b5a8bcc62b995f";
  libraryHaskellDepends = [
    base data-array-byte deepseq template-haskell transformers
  ];
  testHaskellDepends = [
    base base-orphans ghc-prim QuickCheck quickcheck-classes-base
    tagged tasty tasty-quickcheck transformers transformers-compat
  ];
  benchmarkHaskellDepends = [
    base deepseq tasty-bench transformers
  ];
  doCheck = false;
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = lib.licenses.bsd3;
}
