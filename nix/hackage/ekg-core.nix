{ mkDerivation, base, containers, ghc-prim, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "ekg-core";
  version = "0.1.1.7";
  sha256 = "45813f2b94fde0b92c7979bd37de52f09b8b645560f5789276c3acfc7934db12";
  revision = "1";
  editedCabalFile = "17rfxsns0za7jqp3069mwp0lbd433gwb7lrnla02y7hfxbpnldf4";
  libraryHaskellDepends = [
    base containers ghc-prim text unordered-containers
  ];
  benchmarkHaskellDepends = [ base ];
  homepage = "https://github.com/tibbe/ekg-core";
  description = "Tracking of system metrics";
  license = stdenv.lib.licenses.bsd3;
}
