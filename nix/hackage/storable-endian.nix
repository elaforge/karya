{ mkDerivation, base, byteorder, lib }:
mkDerivation {
  pname = "storable-endian";
  version = "0.2.6";
  sha256 = "3743ac8f084ed3187b83f17b4fac280e77c5df01f7910f42b6a1bf09d5a65489";
  revision = "1";
  editedCabalFile = "12f8sscsvsarlwz3p6kk9vbvqsbyhs8lhafgn9h7c0z6pz1amrya";
  libraryHaskellDepends = [ base byteorder ];
  description = "Storable instances with endianness";
  license = lib.licenses.bsd3;
}
