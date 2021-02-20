{ mkDerivation, assoc, base, binary, bytestring, deepseq, ghc-prim
, hashable, stdenv, text, these, transformers
}:
mkDerivation {
  pname = "strict";
  version = "0.4.0.1";
  sha256 = "dff6abc08ad637e51891bb8b475778c40926c51219eda60fd64f0d9680226241";
  libraryHaskellDepends = [
    assoc base binary bytestring deepseq ghc-prim hashable text these
    transformers
  ];
  homepage = "https://github.com/haskell-strict/strict";
  description = "Strict data types and String IO";
  license = stdenv.lib.licenses.bsd3;
}
