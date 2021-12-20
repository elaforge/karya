{ mkDerivation, base, lib, transformers }:
mkDerivation {
  pname = "writer-cps-transformers";
  version = "0.5.6.1";
  sha256 = "76eacf1c3df8f86b6d11507219d7e840d7fb2898f53959aa3dad40791b8f321c";
  libraryHaskellDepends = [ base transformers ];
  doHaddock = false;
  homepage = "https://github.com/minad/writer-cps-transformers#readme";
  description = "WriteT and RWST monad transformers";
  license = lib.licenses.bsd3;
}
