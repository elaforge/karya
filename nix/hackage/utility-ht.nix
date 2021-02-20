{ mkDerivation, base, QuickCheck, stdenv }:
mkDerivation {
  pname = "utility-ht";
  version = "0.0.15";
  sha256 = "845c79cdf0925997ec190a761460b50a9577cd290567b9252e31d075cd6f0f91";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base QuickCheck ];
  description = "Various small helper functions for Lists, Maybes, Tuples, Functions";
  license = stdenv.lib.licenses.bsd3;
}
