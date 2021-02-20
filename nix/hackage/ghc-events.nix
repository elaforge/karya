{ mkDerivation, array, base, binary, bytestring, containers, stdenv
, text, vector
}:
mkDerivation {
  pname = "ghc-events";
  version = "0.15.1";
  sha256 = "bf5f017d4f462f2116610f55684cacaff94dbf4fbbd22497a9a30c603e31709a";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base binary bytestring containers text vector
  ];
  executableHaskellDepends = [ base containers ];
  testHaskellDepends = [ base ];
  description = "Library and tool for parsing .eventlog files from GHC";
  license = stdenv.lib.licenses.bsd3;
}
