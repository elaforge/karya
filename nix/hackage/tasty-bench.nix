{ mkDerivation, base, containers, deepseq, stdenv, tasty }:
mkDerivation {
  pname = "tasty-bench";
  version = "0.2.5";
  sha256 = "edc5a39eacf267948ad34c6382a29c45c68171aa287a7c6684088074e70ed190";
  revision = "1";
  editedCabalFile = "0rcsdiwri52wng5dj30k3c5qrn8qfr14qs53cs1y99mbqfpzs02g";
  libraryHaskellDepends = [ base containers deepseq tasty ];
  homepage = "https://github.com/Bodigrim/tasty-bench";
  description = "Featherlight benchmark framework";
  license = stdenv.lib.licenses.mit;
}
