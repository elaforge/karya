{ mkDerivation, base, bytestring, QuickCheck, stdenv, tasty
, tasty-hunit, tasty-quickcheck, zlib
}:
mkDerivation {
  pname = "zlib";
  version = "0.6.2.3";
  sha256 = "807f6bddf9cb3c517ce5757d991dde3c7e319953a22c86ee03d74534bd5abc88";
  libraryHaskellDepends = [ base bytestring ];
  librarySystemDepends = [ zlib ];
  testHaskellDepends = [
    base bytestring QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  description = "Compression and decompression in the gzip and zlib formats";
  license = stdenv.lib.licenses.bsd3;
}
