{ mkDerivation, base, directory, extra, filepath, QuickCheck
, lib
}:
mkDerivation {
  pname = "filepattern";
  version = "0.1.2";
  sha256 = "d92912ee0db0b8c50d6b2ffdc1ae91ee30e2704b47896aa325b42b58a2fcf65b";
  libraryHaskellDepends = [ base directory extra filepath ];
  testHaskellDepends = [ base directory extra filepath QuickCheck ];
  homepage = "https://github.com/ndmitchell/filepattern#readme";
  description = "File path glob-like matching";
  license = lib.licenses.bsd3;
}
