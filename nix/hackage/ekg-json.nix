{ mkDerivation, aeson, base, ekg-core, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "ekg-json";
  version = "0.1.0.6";
  sha256 = "1e6a80aa0a28bbf41c9c6364cbb5731160d14fa54145f27a82d0b3467a04dd47";
  revision = "7";
  editedCabalFile = "1f53dh7h48j07xw4jdxzwipndap8wdg36d857zdkaxmf14dzqvp1";
  libraryHaskellDepends = [
    aeson base ekg-core text unordered-containers
  ];
  homepage = "https://github.com/tibbe/ekg-json";
  description = "JSON encoding of ekg metrics";
  license = stdenv.lib.licenses.bsd3;
}
