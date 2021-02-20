{ mkDerivation, base, base-orphans, comonad, containers, hspec
, hspec-discover, QuickCheck, stdenv, tagged, template-haskell
, th-abstraction, transformers, transformers-compat
}:
mkDerivation {
  pname = "bifunctors";
  version = "5.5.8";
  sha256 = "817c496af1fa0d015b30630808684d62c73e0da1e51e49de8e92f65b7bddce3d";
  libraryHaskellDepends = [
    base base-orphans comonad containers tagged template-haskell
    th-abstraction transformers
  ];
  testHaskellDepends = [
    base hspec QuickCheck template-haskell transformers
    transformers-compat
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/ekmett/bifunctors/";
  description = "Bifunctors";
  license = stdenv.lib.licenses.bsd3;
}
