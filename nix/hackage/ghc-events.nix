{ mkDerivation, array, base, binary, bytestring, containers, lib
, text, vector
}:
mkDerivation {
  pname = "ghc-events";
  version = "0.17.0.1";
  sha256 = "599f814bd0730716df0f1652ea32d6faa6d61438bb091ef90d2caf7c39a26ec9";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base binary bytestring containers text vector
  ];
  executableHaskellDepends = [ base containers ];
  testHaskellDepends = [ base ];
  doCheck = false;
  description = "Library and tool for parsing .eventlog files from GHC";
  license = lib.licenses.bsd3;
}
