{ mkDerivation, attoparsec, base, bytestring, bytestring-builder
, deepseq, directory, filepath, HUnit, mtl, network, primitive
, process, QuickCheck, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, time, transformers, vector
, zlib, zlib-bindings
}:
mkDerivation {
  pname = "io-streams";
  version = "1.5.1.0";
  sha256 = "8aa2f0afb83ee2e43c472509ecd45475b9ed4409dfaec16eb781f44152f6ebb0";
  revision = "1";
  editedCabalFile = "0n0qs5lgryh3zxy73j3qbpnxand43yd2bj6pclvyii8apqqp901a";
  configureFlags = [ "-fNoInteractiveTests" ];
  libraryHaskellDepends = [
    attoparsec base bytestring bytestring-builder network primitive
    process text time transformers vector zlib-bindings
  ];
  testHaskellDepends = [
    attoparsec base bytestring bytestring-builder deepseq directory
    filepath HUnit mtl network primitive process QuickCheck
    test-framework test-framework-hunit test-framework-quickcheck2 text
    time transformers vector zlib zlib-bindings
  ];
  description = "Simple, composable, and easy-to-use stream I/O";
  license = stdenv.lib.licenses.bsd3;
}
