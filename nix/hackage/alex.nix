{ mkDerivation, array, base, containers, directory, happy, process
, stdenv
}:
mkDerivation {
  pname = "alex";
  version = "3.2.5";
  sha256 = "b77c8a1270767c64e2adb21a6e91ee7cd904ba17edae17bc20fd03da5256e0e3";
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [ array base containers directory ];
  executableToolDepends = [ happy ];
  testHaskellDepends = [ base process ];
  homepage = "http://www.haskell.org/alex/";
  description = "Alex is a tool for generating lexical analysers in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
