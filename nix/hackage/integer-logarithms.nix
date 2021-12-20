{ mkDerivation, array, base, ghc-prim, integer-gmp, QuickCheck
, smallcheck, lib, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck
}:
mkDerivation {
  pname = "integer-logarithms";
  version = "1.0.3.1";
  sha256 = "9b0a9f9fab609b15cd015865721fb05f744a1bc77ae92fd133872de528bbea7f";
  libraryHaskellDepends = [ array base ghc-prim integer-gmp ];
  testHaskellDepends = [
    base QuickCheck smallcheck tasty tasty-hunit tasty-quickcheck
    tasty-smallcheck
  ];
  homepage = "https://github.com/haskellari/integer-logarithms";
  description = "Integer logarithms";
  license = lib.licenses.mit;
}
