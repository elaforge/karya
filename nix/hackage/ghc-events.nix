{ mkDerivation, array, base, binary, bytestring, containers, lib
, text, vector
}:
mkDerivation {
  pname = "ghc-events";
  version = "0.19.0.1";
  sha256 = "c1ae2981b3a9a54b0fb5f8eaac7485dc79f4c2b5a521ec350bf01e51bacf91e6";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base binary bytestring containers text vector
  ];
  executableHaskellDepends = [ base bytestring containers ];
  testHaskellDepends = [ base ];
  doCheck = false;
  description = "Library and tool for parsing .eventlog files from GHC";
  license = lib.licenses.bsd3;
  mainProgram = "ghc-events";
}
