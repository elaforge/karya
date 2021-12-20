{ mkDerivation, base, containers, ghc-prim, mmorph, mtl, lib
, transformers, transformers-base
}:
mkDerivation {
  pname = "streaming";
  version = "0.2.3.0";
  sha256 = "b4008eee1fcee6a9f63d0d31eebefd6cf72731fab65d943831338c3961fafd62";
  libraryHaskellDepends = [
    base containers ghc-prim mmorph mtl transformers transformers-base
  ];
  homepage = "https://github.com/haskell-streaming/streaming";
  description = "an elementary streaming prelude and general stream type";
  license = lib.licenses.bsd3;
}
