{ mkDerivation, base, base-orphans, criterion, HUnit, mtl, lib
, template-haskell, transformers
}:
mkDerivation {
  pname = "fclabels";
  version = "2.0.5";
  sha256 = "3d9ba837ff8141922fc431d1dfd12052f8113b0f2b9ae68d47a3ef557460fb5e";
  libraryHaskellDepends = [
    base base-orphans mtl template-haskell transformers
  ];
  testHaskellDepends = [
    base HUnit mtl template-haskell transformers
  ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/sebastiaanvisser/fclabels";
  description = "First class accessor labels implemented as lenses";
  license = lib.licenses.bsd3;
}
