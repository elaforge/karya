{ mkDerivation, ansi-terminal, async, base, directory, exceptions
, process, stdenv, stm, terminal-size, text, transformers, unix
}:
mkDerivation {
  pname = "concurrent-output";
  version = "1.10.11";
  sha256 = "e30a5f8164f6e172349c4f882455d4e28d6b007443f308562a9304541e562ab4";
  libraryHaskellDepends = [
    ansi-terminal async base directory exceptions process stm
    terminal-size text transformers unix
  ];
  description = "Ungarble output from several threads or commands";
  license = stdenv.lib.licenses.bsd2;
}
