{ mkDerivation, base, bytestring, stdenv, zlib }:
mkDerivation {
  pname = "digest";
  version = "0.0.1.2";
  sha256 = "641717eb16392abf8965986a9e8dc21eebf1d97775bbb6923c7b7f8fee17fe11";
  libraryHaskellDepends = [ base bytestring ];
  librarySystemDepends = [ zlib ];
  description = "Various cryptographic hashes for bytestrings; CRC32 and Adler32 for now";
  license = stdenv.lib.licenses.bsd3;
}
