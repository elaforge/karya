{ mkDerivation, base, bytestring, stdenv, text }:
mkDerivation {
  pname = "readable";
  version = "0.3.1";
  sha256 = "703037ad2cca4d6d42ba23e2758d1911cd82e3e922c4078076c273231e4b43c9";
  libraryHaskellDepends = [ base bytestring text ];
  homepage = "https://github.com/mightybyte/readable";
  description = "Reading from Text and ByteString";
  license = stdenv.lib.licenses.bsd3;
}
