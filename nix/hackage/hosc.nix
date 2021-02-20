{ mkDerivation, base, binary, blaze-builder, bytestring
, data-binary-ieee754, network, stdenv, time, transformers
}:
mkDerivation {
  pname = "hosc";
  version = "0.17";
  sha256 = "66439c416246cb56c15a0f3fb0cf07b178202c7755034b648f02d4f81ba5800c";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base binary blaze-builder bytestring data-binary-ieee754 network
    time transformers
  ];
  homepage = "http://rohandrape.net/t/hosc";
  description = "Haskell Open Sound Control";
  license = stdenv.lib.licenses.gpl3;
}
