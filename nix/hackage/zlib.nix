{ mkDerivation, base, bytestring, QuickCheck, stdenv, tasty
, tasty-hunit, tasty-quickcheck, zlib
}:
mkDerivation {
  pname = "zlib";
  version = "0.6.2.1";
  sha256 = "f0f810ff173560b60392db448455c0513b3239f48e43cb494b3733aa559621d0";
  revision = "1";
  editedCabalFile = "0i9g71jvdw22bi9bi8dm5khwzcsv6cv8yadmf7afklg4xigxykfk";
  libraryHaskellDepends = [ base bytestring ];
  librarySystemDepends = [ zlib ];
  testHaskellDepends = [
    base bytestring QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  description = "Compression and decompression in the gzip and zlib formats";
  license = stdenv.lib.licenses.bsd3;
}
