{ mkDerivation, base, binary, bytestring, deepseq, ghc-prim
, hashable, lib, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text
}:
mkDerivation {
  pname = "text-short";
  version = "0.1.5";
  sha256 = "a35ec6cde2ada084c1a050dc5885be5ab01f851b93d744cf0facbc1c18002dda";
  libraryHaskellDepends = [
    base binary bytestring deepseq ghc-prim hashable template-haskell
    text
  ];
  testHaskellDepends = [
    base binary bytestring tasty tasty-hunit tasty-quickcheck
    template-haskell text
  ];
  doCheck = false;
  description = "Memory-efficient representation of Unicode text strings";
  license = lib.licenses.bsd3;
}
