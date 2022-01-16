{ mkDerivation, async, base, base-compat, base-compat-batteries
, bytestring, clock, containers, criterion, deepseq, HUnit, lib
, math-functions, process, random, test-framework
, test-framework-hunit, tf-random, vector
}:
mkDerivation {
  pname = "splitmix";
  version = "0.1.0.4";
  sha256 = "6d065402394e7a9117093dbb4530a21342c9b1e2ec509516c8a8d0ffed98ecaa";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [
    async base base-compat base-compat-batteries bytestring containers
    deepseq HUnit math-functions process random test-framework
    test-framework-hunit tf-random vector
  ];
  # testSystemDepends = [ testu01 ];
  benchmarkHaskellDepends = [
    base clock containers criterion random tf-random
  ];
  doCheck = false;
  description = "Fast Splittable PRNG";
  license = lib.licenses.bsd3;
}
