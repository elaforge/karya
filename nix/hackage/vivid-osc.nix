{ mkDerivation, base, bytestring, cereal, microspec, lib, time
}:
mkDerivation {
  pname = "vivid-osc";
  version = "0.5.0.0";
  sha256 = "46fb67915fdfa37db0ff620b1529caf77a19d41a71007aae2b834facc2243510";
  libraryHaskellDepends = [ base bytestring cereal time ];
  testHaskellDepends = [ base bytestring cereal microspec time ];
  doCheck = false;
  description = "Open Sound Control encode/decode";
  license = "GPL";
}
