{ mkDerivation, array, base, bytestring, case-insensitive
, containers, data-fix, hashable, integer-logarithms, lib, old-time
, OneTuple, QuickCheck, scientific, splitmix, strict, tagged, text
, text-short, these, time, time-compat, transformers
, transformers-compat, unordered-containers, uuid-types, vector
}:
mkDerivation {
  pname = "quickcheck-instances";
  version = "0.3.27";
  sha256 = "a592dd5b4320a9a08d26eea425349ecfce241fb23b41e61b263da91a681e6b83";
  revision = "1";
  editedCabalFile = "1b17ghhhrw9h625q08145pdpw72xmava73d3xb933slza5jms6nz";
  libraryHaskellDepends = [
    array base bytestring case-insensitive containers data-fix hashable
    integer-logarithms old-time OneTuple QuickCheck scientific splitmix
    strict tagged text text-short these time time-compat transformers
    transformers-compat unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    base containers QuickCheck tagged uuid-types
  ];
  benchmarkHaskellDepends = [ base bytestring QuickCheck ];
  doCheck = false;
  homepage = "https://github.com/haskellari/qc-instances";
  description = "Common quickcheck instances";
  license = lib.licenses.bsd3;
}
