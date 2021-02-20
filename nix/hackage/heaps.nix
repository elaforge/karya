{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "heaps";
  version = "0.3.6.1";
  sha256 = "91d552f3c8992f745607de39239b950db78295b533eda43d083699872a4ee36d";
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/ekmett/heaps/";
  description = "Asymptotically optimal Brodal/Okasaki heaps";
  license = stdenv.lib.licenses.bsd3;
}
