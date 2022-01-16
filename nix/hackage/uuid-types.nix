{ mkDerivation, base, binary, bytestring, deepseq, ghc-byteorder
, hashable, lib, QuickCheck, random, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, text
}:
mkDerivation {
  pname = "uuid-types";
  version = "1.0.5";
  sha256 = "ad68b89b7a64c07dd5c250a11be2033ee929318ff51ec7b4e4b54e1b4deba7dd";
  revision = "2";
  editedCabalFile = "0x3limqb67l4i0lfdaqgqbjak7mi7ydk5dhkv80791r3hyhbhiw4";
  libraryHaskellDepends = [
    base binary bytestring deepseq hashable random template-haskell
    text
  ];
  testHaskellDepends = [
    base binary bytestring ghc-byteorder QuickCheck tasty tasty-hunit
    tasty-quickcheck template-haskell
  ];
  doCheck = false;
  homepage = "https://github.com/haskell-hvr/uuid";
  description = "Type definitions for Universally Unique Identifiers";
  license = lib.licenses.bsd3;
}
