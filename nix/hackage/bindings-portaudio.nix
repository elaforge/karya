{ mkDerivation, base, bindings-DSL, portaudio, stdenv, vector }:
mkDerivation {
  pname = "bindings-portaudio";
  version = "0.3";
  sha256 = "20e97ba3957263485b342c68fd94836be6903e056935b95a59e6e97bfaac1869";
  libraryHaskellDepends = [ base bindings-DSL vector ];
  libraryPkgconfigDepends = [ portaudio ];
  description = "Low-level bindings to portaudio library";
  license = stdenv.lib.licenses.bsd3;
}
