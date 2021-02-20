{ mkDerivation, attoparsec, base, bytestring, bytestring-builder
, case-insensitive, containers, deepseq, directory, filepath
, hashable, HUnit, io-streams, lifted-base, monad-control, mtl
, network, network-uri, old-locale, parallel, QuickCheck, random
, readable, regex-posix, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, time
, transformers, transformers-base, unix-compat
, unordered-containers, vector, zlib
}:
mkDerivation {
  pname = "snap-core";
  version = "1.0.4.1";
  sha256 = "c0b177d47fcee1923d08bdba5b5f975d54e5e495ca666be5cc617aa71776b5a5";
  revision = "1";
  editedCabalFile = "0m8vsgrj96a0y9h09szg7gxv9f26yizh4k181ri2sp7ki8p5p7lg";
  libraryHaskellDepends = [
    attoparsec base bytestring bytestring-builder case-insensitive
    containers directory filepath hashable HUnit io-streams lifted-base
    monad-control mtl network network-uri old-locale random readable
    regex-posix text time transformers transformers-base unix-compat
    unordered-containers vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring bytestring-builder case-insensitive
    containers deepseq directory filepath hashable HUnit io-streams
    lifted-base monad-control mtl network network-uri old-locale
    parallel QuickCheck random readable regex-posix test-framework
    test-framework-hunit test-framework-quickcheck2 text time
    transformers transformers-base unix-compat unordered-containers
    vector zlib
  ];
  homepage = "http://snapframework.com/";
  description = "Snap: A Haskell Web Framework (core interfaces and types)";
  license = stdenv.lib.licenses.bsd3;
}
