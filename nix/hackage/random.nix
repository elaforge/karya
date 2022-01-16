{ mkDerivation, base, bytestring, containers, deepseq, doctest, lib
, mtl, primitive, rdtsc, smallcheck, split, splitmix, stm, tasty
, tasty-bench, tasty-hunit, tasty-inspection-testing
, tasty-smallcheck, time, transformers
}:
mkDerivation {
  pname = "random";
  version = "1.2.1";
  sha256 = "265c768fc5f2ca53cde6a87e706b4448cad474c3deece933c103f24453661457";
  libraryHaskellDepends = [ base bytestring deepseq mtl splitmix ];
  testHaskellDepends = [
    base bytestring containers doctest smallcheck stm tasty tasty-hunit
    tasty-inspection-testing tasty-smallcheck transformers
  ];
  benchmarkHaskellDepends = [
    base mtl primitive rdtsc split splitmix tasty-bench time
  ];
  doCheck = false;
  description = "Pseudo-random number generation";
  license = lib.licenses.bsd3;
}
