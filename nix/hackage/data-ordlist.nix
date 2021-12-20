{ mkDerivation, base, lib }:
mkDerivation {
  pname = "data-ordlist";
  version = "0.4.7.0";
  sha256 = "6f6c1e7a9a9155ad78ca78cb9abd6f7e2e1c78b3e549b179dc0874e6428f490d";
  libraryHaskellDepends = [ base ];
  description = "Set and bag operations on ordered lists";
  license = lib.licenses.bsd3;
}
