{ mkDerivation, base, deepseq, hashable, lib }:
mkDerivation {
  pname = "data-fix";
  version = "0.3.2";
  sha256 = "3a172d3bc0639c327345e965f9d9023e099425814b28dcdb7b60ff66d66219cc";
  revision = "2";
  editedCabalFile = "0ymn341kg2c1wf1vp04v25bpnf857krhv91q4kl7b2k4h5ipf2g9";
  libraryHaskellDepends = [ base deepseq hashable ];
  doCheck = false;
  homepage = "https://github.com/spell-music/data-fix";
  description = "Fixpoint data types";
  license = lib.licenses.bsd3;
}
