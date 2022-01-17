{ mkDerivation, base, ghc-prim, hspec, hspec-discover, lib
, QuickCheck
}:
mkDerivation {
  pname = "base-orphans";
  version = "0.8.6";
  sha256 = "20a21c4b7adb0fd844b25e196241467406a28286b021f9b7a082ab03fa8015eb";
  libraryHaskellDepends = [ base ghc-prim ];
  testHaskellDepends = [ base hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  doCheck = false;
  homepage = "https://github.com/haskell-compat/base-orphans#readme";
  description = "Backwards-compatible orphan instances for base";
  license = lib.licenses.mit;
}
