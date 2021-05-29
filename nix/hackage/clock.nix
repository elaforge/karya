{ mkDerivation, base, criterion, stdenv, tasty, tasty-quickcheck }:
mkDerivation {
  pname = "clock";
  version = "0.8.2";
  sha256 = "0b5db110c703e68b251d5883253a934b012110b45393fc65df1b095eb9a4e461";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base tasty tasty-quickcheck ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/corsis/clock";
  description = "High-resolution clock functions: monotonic, realtime, cputime";
  license = stdenv.lib.licenses.bsd3;
}
