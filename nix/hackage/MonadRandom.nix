{ mkDerivation, base, mtl, primitive, random, lib, transformers
, transformers-compat
}:
mkDerivation {
  pname = "MonadRandom";
  version = "0.5.3";
  sha256 = "27184dadda0a49abac0208a1e6576b14217a60dc45b6839cd9e90af25ee00a9f";
  revision = "1";
  editedCabalFile = "1wpgmcv704i7x38jwalnbmx8c10vdw269gbvzjxaj4rlvff3s4sq";
  libraryHaskellDepends = [
    base mtl primitive random transformers transformers-compat
  ];
  description = "Random-number generation monad";
  license = lib.licenses.bsd3;
}
