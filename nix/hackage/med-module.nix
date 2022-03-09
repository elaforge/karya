{ mkDerivation, base, bytestring, lib, storable-endian
, transformers, utility-ht
}:
mkDerivation {
  pname = "med-module";
  version = "0.1.2.2";
  sha256 = "6aa533c3f2c0d1c21c5148deaeb27d15b6b53f96ffb004e7846b91df13f97c63";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring storable-endian transformers utility-ht
  ];
  doCheck = false;
  description = "Parse song module files from Amiga MED and OctaMED";
  license = lib.licenses.gpl3;
}
