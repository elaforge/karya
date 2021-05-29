{ mkDerivation, base, clock, directory, filepath, process
, QuickCheck, quickcheck-instances, stdenv, time, unix
}:
mkDerivation {
  pname = "extra";
  version = "1.7.9";
  sha256 = "f66e26a63b216f0ca33665a75c08eada0a96af192ace83a18d87839d79afdf9d";
  libraryHaskellDepends = [
    base clock directory filepath process time unix
  ];
  testHaskellDepends = [
    base directory filepath QuickCheck quickcheck-instances unix
  ];
  homepage = "https://github.com/ndmitchell/extra#readme";
  description = "Extra functions I use";
  license = stdenv.lib.licenses.bsd3;
}
