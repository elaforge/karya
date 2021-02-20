{ mkDerivation, aeson, base, bytestring, ekg-core, ekg-json
, filepath, network, snap-core, snap-server, stdenv, text, time
, transformers, unordered-containers
}:
mkDerivation {
  pname = "ekg";
  version = "0.4.0.15";
  sha256 = "482ae3be495cfe4f03332ad1c79ce8b5ad4f9c8eec824980c664808ae32c6dcc";
  revision = "8";
  editedCabalFile = "05k50vx956zlh7dvkhi7qvd9f7x48hg5hwgqjqsf5fwzm1cqir6n";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base bytestring ekg-core ekg-json filepath network snap-core
    snap-server text time transformers unordered-containers
  ];
  homepage = "https://github.com/tibbe/ekg";
  description = "Remote monitoring of processes";
  license = stdenv.lib.licenses.bsd3;
}
