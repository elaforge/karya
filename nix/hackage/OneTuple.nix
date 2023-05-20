{ mkDerivation, base, foldable1-classes-compat, ghc-prim, hashable
, lib, template-haskell
}:
mkDerivation {
  pname = "OneTuple";
  version = "0.4.1.1";
  sha256 = "c9e764d4ee1e57cad8341bd5d0de33ba3a52b6793fc1309679f2bf60c030bb2b";
  libraryHaskellDepends = [ base ghc-prim template-haskell ];
  testHaskellDepends = [
    base foldable1-classes-compat hashable template-haskell
  ];
  doCheck = false;
  description = "Singleton Tuple";
  license = lib.licenses.bsd3;
}
