{ mkDerivation, base, bytestring, containers, deepseq, doctest
, gauge, mtl, mwc-random, primitive, rdtsc, smallcheck, split
, splitmix, stdenv, tasty, tasty-expected-failure, tasty-hunit
, tasty-smallcheck, time, unliftio, vector
}:
mkDerivation {
  pname = "random";
  version = "1.2.0";
  sha256 = "e4519cf7c058bfd5bdbe4acc782284acc9e25e74487208619ca83cbcd63fb9de";
  revision = "5";
  editedCabalFile = "1jai1pcs39ijdhxc8q36x1yayr8rsblhx3y88paf4bqxrks2vmrh";
  libraryHaskellDepends = [ base bytestring deepseq mtl splitmix ];
  testHaskellDepends = [
    base bytestring containers doctest mwc-random primitive smallcheck
    tasty tasty-expected-failure tasty-hunit tasty-smallcheck unliftio
    vector
  ];
  benchmarkHaskellDepends = [
    base gauge mtl rdtsc split splitmix time
  ];
  description = "Pseudo-random number generation";
  license = stdenv.lib.licenses.bsd3;
}
