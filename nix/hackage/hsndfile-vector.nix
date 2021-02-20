{ mkDerivation, base, hsndfile, stdenv, vector }:
mkDerivation {
  pname = "hsndfile-vector";
  version = "0.5.2";
  sha256 = "2ffe11eb9a3e63aae24e8e06d2e04e8ca4a617ccf1238843cc71517a905b2895";
  libraryHaskellDepends = [ base hsndfile vector ];
  homepage = "http://haskell.org/haskellwiki/Hsndfile";
  description = "Haskell bindings for libsndfile (Data.Vector interface)";
  license = stdenv.lib.licenses.lgpl2;
}
