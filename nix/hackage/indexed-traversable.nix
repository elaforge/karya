{ mkDerivation, array, base, containers, lib, transformers }:
mkDerivation {
  pname = "indexed-traversable";
  version = "0.1.2";
  sha256 = "516858ee7198b1fed1b93c665157f9855fd947379db7f115d48c1b0d670e698d";
  revision = "1";
  editedCabalFile = "0rbcfl0iklix3ppfkxh88y70qmm64lg1l4679z5krya2fa42hqnn";
  libraryHaskellDepends = [ array base containers transformers ];
  description = "FunctorWithIndex, FoldableWithIndex, TraversableWithIndex";
  license = lib.licenses.bsd2;
}
