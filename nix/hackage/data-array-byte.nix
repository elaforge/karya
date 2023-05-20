{ mkDerivation, base, deepseq, lib, quickcheck-classes-base, tasty
, tasty-quickcheck, template-haskell
}:
mkDerivation {
  pname = "data-array-byte";
  version = "0.1.0.1";
  sha256 = "1bb6eca0b3e02d057fe7f4e14c81ef395216f421ab30fdaa1b18017c9c025600";
  revision = "1";
  editedCabalFile = "1nma7gz7lhain6jvwb3w3s53716ss8ypkk93gxpsaaz824svvw9f";
  libraryHaskellDepends = [ base deepseq template-haskell ];
  testHaskellDepends = [
    base quickcheck-classes-base tasty tasty-quickcheck
    template-haskell
  ];
  doCheck = false;
  homepage = "https://github.com/Bodigrim/data-array-byte";
  description = "Compatibility layer for Data.Array.Byte";
  license = lib.licenses.bsd3;
}
