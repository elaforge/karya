{ mkDerivation, base, ghc-prim, stdenv, transformers }:
mkDerivation {
  pname = "transformers-compat";
  version = "0.6.6";
  sha256 = "7e2e0251e5e6d28142615a4b950a3fabac9c0b7804b1ec4a4ae985f19519a9f9";
  libraryHaskellDepends = [ base ghc-prim transformers ];
  homepage = "http://github.com/ekmett/transformers-compat/";
  description = "A small compatibility shim for the transformers library";
  license = stdenv.lib.licenses.bsd3;
}
