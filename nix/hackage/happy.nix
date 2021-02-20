{ mkDerivation, array, base, containers, mtl, process, stdenv }:
mkDerivation {
  pname = "happy";
  version = "1.19.12";
  sha256 = "fb9a23e41401711a3b288f93cf0a66db9f97da1ce32ec4fffea4b78a0daeb40f";
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [ array base containers mtl ];
  testHaskellDepends = [ base process ];
  homepage = "https://www.haskell.org/happy/";
  description = "Happy is a parser generator for Haskell";
  license = stdenv.lib.licenses.bsd2;
}
