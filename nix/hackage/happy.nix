{ mkDerivation, array, base, containers, mtl, process, stdenv }:
mkDerivation {
  pname = "happy";
  version = "1.20.0";
  sha256 = "3b1d3a8f93a2723b554d9f07b2cd136be1a7b2fcab1855b12b7aab5cbac8868c";
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [ array base containers mtl ];
  testHaskellDepends = [ base process ];
  homepage = "https://www.haskell.org/happy/";
  description = "Happy is a parser generator for Haskell";
  license = stdenv.lib.licenses.bsd2;
}
