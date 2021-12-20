{ mkDerivation, base, lib }:
mkDerivation {
  pname = "semigroups";
  version = "0.19.1";
  sha256 = "79e761e64b862564a3470d5d356cb6b060b14452d675859aed3b2d1e14646648";
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/ekmett/semigroups/";
  description = "Anything that associates";
  license = lib.licenses.bsd3;
}
