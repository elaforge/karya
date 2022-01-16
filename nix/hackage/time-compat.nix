{ mkDerivation, base, base-compat, base-orphans, deepseq, hashable
, HUnit, lib, QuickCheck, tagged, tasty, tasty-hunit
, tasty-quickcheck, time
}:
mkDerivation {
  pname = "time-compat";
  version = "1.9.6.1";
  sha256 = "ad07bb00eb9678c2136d3680752b00acc4cbc522654bb3199bf31c61ef1e6b80";
  revision = "3";
  editedCabalFile = "1lafp8yk2n8g873ivi36gnwd8syhw5lssm3xj4c1fplnivhg5n22";
  libraryHaskellDepends = [
    base base-orphans deepseq hashable time
  ];
  testHaskellDepends = [
    base base-compat deepseq hashable HUnit QuickCheck tagged tasty
    tasty-hunit tasty-quickcheck time
  ];
  homepage = "https://github.com/haskellari/time-compat";
  description = "Compatibility package for time";
  license = lib.licenses.bsd3;
}
