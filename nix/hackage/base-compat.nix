{ mkDerivation, base, lib, unix }:
mkDerivation {
  pname = "base-compat";
  version = "0.11.2";
  sha256 = "53a6b5145442fba5a4bad6db2bcdede17f164642b48bc39b95015422a39adbdb";
  libraryHaskellDepends = [ base unix ];
  description = "A compatibility layer for base";
  license = lib.licenses.mit;
}
