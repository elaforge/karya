{ mkDerivation, base, binary, bytestring, cereal, microspec
, QuickCheck, split, stdenv, utf8-string, vivid-osc
}:
mkDerivation {
  pname = "vivid-supercollider";
  version = "0.4.1.2";
  sha256 = "d2a40e8f4fff13200e6ead4d6397fe31095d990f75616bf7f89dbf3fa81821cb";
  libraryHaskellDepends = [
    base binary bytestring cereal split utf8-string vivid-osc
  ];
  testHaskellDepends = [
    base binary bytestring cereal microspec QuickCheck utf8-string
    vivid-osc
  ];
  doCheck = false;
  description = "Implementation of SuperCollider server specifications";
  license = "GPL";
}
