{ mkDerivation, array, base, containers, lib, transformers }:
mkDerivation {
  pname = "indexed-traversable";
  version = "0.1.2.1";
  sha256 = "fe854c10285debc7d6fe3e09da0928a740ebc091ad2911ae695bb007e6f746a4";
  libraryHaskellDepends = [ array base containers transformers ];
  doCheck = false;
  description = "FunctorWithIndex, FoldableWithIndex, TraversableWithIndex";
  license = lib.licenses.bsd2;
}
