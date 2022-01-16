{ mkDerivation, base, base-orphans, criterion, HUnit, lib, mtl
, template-haskell, transformers
}:
mkDerivation {
  pname = "fclabels";
  version = "2.0.5.1";
  sha256 = "939c4075fb2aeb0ea69d6d8e252dd2b8c4743cc4fcc4acaed54e2d516f518c3c";
  revision = "1";
  editedCabalFile = "03df1mvfsnm4zbppvig7y49y935qxvkz4b5y24b17hsdj4l1nnyk";
  libraryHaskellDepends = [
    base base-orphans mtl template-haskell transformers
  ];
  testHaskellDepends = [
    base HUnit mtl template-haskell transformers
  ];
  benchmarkHaskellDepends = [ base criterion ];
  doCheck = false;
  homepage = "https://github.com/sebastiaanvisser/fclabels";
  description = "First class accessor labels implemented as lenses";
  license = lib.licenses.bsd3;
}
