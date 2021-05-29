{ mkDerivation, assoc, base, binary, deepseq, hashable, stdenv }:
mkDerivation {
  pname = "these";
  version = "1.1.1.1";
  sha256 = "d798c9f56e17def441e8f51e54cc11afdb3e76c6a9d1e9ee154e9a78da0bf508";
  revision = "1";
  editedCabalFile = "1bzi28jvaxil9rc6z1hkf87pfjsa3r5gfc9n0ixffnnv519cd0g9";
  libraryHaskellDepends = [ assoc base binary deepseq hashable ];
  homepage = "https://github.com/isomorphism/these";
  description = "An either-or-both data type";
  license = stdenv.lib.licenses.bsd3;
}
