{ mkDerivation, base, directory, lib, polyparse, time }:
mkDerivation {
  pname = "cpphs";
  version = "1.20.9.1";
  sha256 = "7f59b10bc3374004cee3c04fa4ee4a1b90d0dca84a3d0e436d5861a1aa3b919f";
  revision = "1";
  editedCabalFile = "1f8jzs8zdh4wwbcq8fy6qqxkv75ypnvsm4yzw49wpr3b9vpnzlha";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory polyparse time ];
  executableHaskellDepends = [ base directory polyparse time ];
  doCheck = false;
  homepage = "http://projects.haskell.org/cpphs/";
  description = "A liberalised re-implementation of cpp, the C pre-processor";
  license = "LGPL";
}
