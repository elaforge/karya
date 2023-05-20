{ mkDerivation, base, containers, deepseq, lib, tasty, tasty-hunit
, text
}:
mkDerivation {
  pname = "wl-pprint-annotated";
  version = "0.1.0.1";
  sha256 = "6b662b244b2e318a2923dc7057d707369a29ea4a0e721b4710eac7239cc727af";
  revision = "1";
  editedCabalFile = "1qizgsiqsraj8w0qndcyw7grcmiylx63vp3lgw2dplchva8p3hp7";
  libraryHaskellDepends = [ base containers deepseq text ];
  testHaskellDepends = [
    base containers deepseq tasty tasty-hunit text
  ];
  doCheck = false;
  homepage = "https://github.com/minad/wl-pprint-annotated#readme";
  description = "Pretty printer with annotation support";
  license = lib.licenses.bsd3;
}
