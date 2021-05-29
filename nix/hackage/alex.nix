{ mkDerivation, array, base, containers, directory, happy, process
, stdenv
}:
mkDerivation {
  pname = "alex";
  version = "3.2.6";
  sha256 = "91aa08c1d3312125fbf4284815189299bbb0be34421ab963b1f2ae06eccc5410";
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
