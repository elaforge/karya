{ mkDerivation, base, bytestring, containers, foldl, gauge
, hashable, hspec, HUnit, mwc-random, QuickCheck, semigroups, split
, stdenv, text, transformers, unordered-containers, vector
, vector-algorithms
}:
mkDerivation {
  pname = "mono-traversable";
  version = "1.0.15.1";
  sha256 = "c2df5b79ed2f88f2ee313e57c1d591d4463788e20d39e439297eec5ba5835ddf";
  libraryHaskellDepends = [
    base bytestring containers hashable split text transformers
    unordered-containers vector vector-algorithms
  ];
  testHaskellDepends = [
    base bytestring containers foldl hspec HUnit QuickCheck semigroups
    text transformers unordered-containers vector
  ];
  benchmarkHaskellDepends = [ base gauge mwc-random vector ];
  homepage = "https://github.com/snoyberg/mono-traversable#readme";
  description = "Type classes for mapping, folding, and traversing monomorphic containers";
  license = stdenv.lib.licenses.mit;
}
