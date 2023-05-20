{ mkDerivation, base, lib, mtl, primitive, random, transformers
, transformers-compat
}:
mkDerivation {
  pname = "MonadRandom";
  version = "0.6";
  sha256 = "c9388630895c3e68035ab804004de852b63a95d083aa140dc0e1481c854ed044";
  revision = "3";
  editedCabalFile = "0v61hlrggnflb9cbpzs3nw8km12scsnvgz9a0gb8lwi26ksgqmnz";
  libraryHaskellDepends = [
    base mtl primitive random transformers transformers-compat
  ];
  doCheck = false;
  description = "Random-number generation monad";
  license = lib.licenses.bsd3;
}
