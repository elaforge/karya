{ mkDerivation, base, doctest-exitcode-stdio, doctest-lib
, QuickCheck, lib
}:
mkDerivation {
  pname = "utility-ht";
  version = "0.0.16";
  sha256 = "bce53223bb77643222331efec5d69a656c0fa2d11be6563e27bc4808a1abbb81";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base doctest-exitcode-stdio doctest-lib QuickCheck
  ];
  description = "Various small helper functions for Lists, Maybes, Tuples, Functions";
  license = lib.licenses.bsd3;
}
