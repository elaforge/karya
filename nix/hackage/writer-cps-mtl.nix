{ mkDerivation, base, mtl, stdenv, transformers
, writer-cps-transformers
}:
mkDerivation {
  pname = "writer-cps-mtl";
  version = "0.1.1.6";
  sha256 = "06f9fb60dc41ad26f3d18089a0b7ff1e1aeb15dc862508c59b6b577c0914dd36";
  libraryHaskellDepends = [
    base mtl transformers writer-cps-transformers
  ];
  homepage = "https://github.com/minad/writer-cps-mtl#readme";
  description = "MonadWriter orphan instances for writer-cps-transformers";
  license = stdenv.lib.licenses.bsd3;
}
