{ mkDerivation, base, distributive, QuickCheck, tasty
, tasty-hunit, tasty-quickcheck, transformers
, lib
}:
mkDerivation {
  pname = "barbies";
  version = "2.0.3.1";
  sha256 = "a13c1fd05e3e006bd0874d77e1930c5225765e83fa9925b2c5c1df314559df3d";
  libraryHaskellDepends = [ base distributive transformers ];
  testHaskellDepends = [
    base distributive QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  doCheck = false;
  homepage = "https://github.com/jcpetruzza/barbies#readme";
  description = "Classes for working with types that can change clothes";
  license = lib.licenses.bsd3;
}
