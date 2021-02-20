{ mkDerivation, base, c2hs, libsndfile, stdenv }:
mkDerivation {
  pname = "hsndfile";
  version = "0.8.0";
  sha256 = "e97e7ef4c26b0dba9400d9a77d3d5001735f64094b93f9733443f58f7f568efb";
  libraryHaskellDepends = [ base ];
  librarySystemDepends = [ libsndfile ];
  libraryToolDepends = [ c2hs ];
  homepage = "http://haskell.org/haskellwiki/Hsndfile";
  description = "Haskell bindings for libsndfile";
  license = stdenv.lib.licenses.lgpl21;
}
