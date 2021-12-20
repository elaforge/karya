{ mkDerivation, alex, array, base, bytestring, containers, deepseq
, directory, filepath, happy, pretty, process, lib, syb
}:
mkDerivation {
  pname = "language-c";
  version = "0.8.3";
  sha256 = "f3d66c18abececb468f2b069f0c3274709456fde6938eace46dacf1c9b14202e";
  libraryHaskellDepends = [
    array base bytestring containers deepseq directory filepath pretty
    process syb
  ];
  libraryToolDepends = [ alex happy ];
  testHaskellDepends = [ base directory filepath process ];
  homepage = "http://visq.github.io/language-c/";
  description = "Analysis and generation of C code";
  license = lib.licenses.bsd3;
}
