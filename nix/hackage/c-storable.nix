{ mkDerivation, base, lib }:
mkDerivation {
  pname = "c-storable";
  version = "0.3";
  sha256 = "e43f4f7fc3670f60cc4ee0d4e2447871bb220caa6d39e851af0bb5a749656944";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/elaforge/c-storable";
  description = "CStorable class";
  license = lib.licenses.bsd3;
}
