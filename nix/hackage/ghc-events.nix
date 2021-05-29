{ mkDerivation, array, base, binary, bytestring, containers, stdenv
, text, vector
}:
mkDerivation {
  pname = "ghc-events";
  version = "0.17.0";
  sha256 = "8cc5b380cdf821b396c237cde6dcf0713d3d355733a9a8fac231a42113d52c15";
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
