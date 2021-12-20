{ mkDerivation, base, colour, lib }:
mkDerivation {
  pname = "ansi-terminal";
  version = "0.11";
  sha256 = "c6611b9e51add41db3f79eac30066c06b33a6ca2a09e586b4b361d7f98303793";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base colour ];
  homepage = "https://github.com/feuerbach/ansi-terminal";
  description = "Simple ANSI terminal support, with Windows compatibility";
  license = lib.licenses.bsd3;
}
