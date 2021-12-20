{ mkDerivation, base, binary, bytestring, containers, lib }:
mkDerivation {
  pname = "zmidi-core";
  version = "0.9.0";
  sha256 = "8511ffa40b077b818e530bdc5ef071806716a2bc239b5fd500389fdbae19ec88";
  libraryHaskellDepends = [ base binary bytestring containers ];
  homepage = "https://github.com/stephentetley/zmidi-core";
  description = "Read and write MIDI files";
  license = lib.licenses.bsd3;
}
