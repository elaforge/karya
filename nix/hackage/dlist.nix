{ mkDerivation, base, deepseq, QuickCheck, lib }:
mkDerivation {
  pname = "dlist";
  version = "1.0";
  sha256 = "173d637328bb173fcc365f30d29ff4a94292a1e0e5558aeb3dfc11de81510115";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [ base QuickCheck ];
  homepage = "https://github.com/spl/dlist";
  description = "Difference lists";
  license = lib.licenses.bsd3;
}
