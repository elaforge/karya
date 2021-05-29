{ mkDerivation, base, base-orphans, deepseq, ghc-prim, QuickCheck
, quickcheck-classes-base, semigroups, stdenv, tagged, tasty
, tasty-quickcheck, transformers, transformers-compat
}:
mkDerivation {
  pname = "primitive";
  version = "0.7.1.0";
  sha256 = "6bebecfdf2a57787d9fd5231bfd612b65a92edd7b33a973b2a0f11312b89a3f0";
  revision = "3";
  editedCabalFile = "03vgkhib8w3g0m0zwpz74hsixrf0pvgh6ql0xcy05fpq1kynppi9";
  libraryHaskellDepends = [ base deepseq transformers ];
  testHaskellDepends = [
    base base-orphans ghc-prim QuickCheck quickcheck-classes-base
    semigroups tagged tasty tasty-quickcheck transformers
    transformers-compat
  ];
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = stdenv.lib.licenses.bsd3;
}
