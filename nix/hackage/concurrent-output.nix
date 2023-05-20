{ mkDerivation, ansi-terminal, async, base, directory, exceptions
, lib, process, stm, terminal-size, text, transformers, unix
}:
mkDerivation {
  pname = "concurrent-output";
  version = "1.10.18";
  sha256 = "b7a8338926d8bd29dd4d2a55f38b0146690c887f29f9df15fa5ec333283facdb";
  libraryHaskellDepends = [
    ansi-terminal async base directory exceptions process stm
    terminal-size text transformers unix
  ];
  doCheck = false;
  description = "Ungarble output from several threads or commands";
  license = lib.licenses.bsd2;
}
