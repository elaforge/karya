{ mkDerivation, base, mtl, QuickCheck, stdenv, stm
, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2, transformers
}:
mkDerivation {
  pname = "exceptions";
  version = "0.10.4";
  sha256 = "4d0bfb4355cffcd67d300811df9d5fe44ea3594ed63750795bfc1f797abd84cf";
  revision = "1";
  editedCabalFile = "0b5m01nmaqzvvm1d07b3dnmcn47pmy943lydb2m7ibhilqkfya8p";
  libraryHaskellDepends = [
    base mtl stm template-haskell transformers
  ];
  testHaskellDepends = [
    base mtl QuickCheck stm template-haskell test-framework
    test-framework-hunit test-framework-quickcheck2 transformers
  ];
  homepage = "http://github.com/ekmett/exceptions/";
  description = "Extensible optionally-pure exceptions";
  license = stdenv.lib.licenses.bsd3;
}
