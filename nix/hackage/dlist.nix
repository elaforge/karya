{ mkDerivation, base, Cabal, deepseq, QuickCheck, stdenv }:
mkDerivation {
  pname = "dlist";
  version = "0.8.0.7";
  sha256 = "a3c06a200f9756329b09d415e7733b7204dda76ce07783e6457d0ab9ffbcba2c";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [ base Cabal QuickCheck ];
  homepage = "https://github.com/spl/dlist";
  description = "Difference lists";
  license = stdenv.lib.licenses.bsd3;
}
