{ mkDerivation, base, byteorder, lib }:
mkDerivation {
  pname = "storable-endian";
  version = "0.2.6.1";
  sha256 = "ceb1a8a9109b837cc66f0a1b61fe2289a224a7afdb69d9952d537098f8709e45";
  libraryHaskellDepends = [ base byteorder ];
  description = "Storable instances with endianness";
  license = lib.licenses.bsd3;
}
