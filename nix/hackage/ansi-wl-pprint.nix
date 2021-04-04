{ mkDerivation, ansi-terminal, base, stdenv }:
mkDerivation {
  pname = "ansi-wl-pprint";
  version = "0.6.9";
  sha256 = "a7b2e8e7cd3f02f2954e8b17dc60a0ccd889f49e2068ebb15abfa1d42f7a4eac";
  revision = "2";
  editedCabalFile = "1xrv66v5hqchjhj8a0g3awy1qpsswk2jqb4w4yh3mm1py5s0dlr0";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ ansi-terminal base ];
  homepage = "http://github.com/ekmett/ansi-wl-pprint";
  description = "The Wadler/Leijen Pretty Printer for colored ANSI terminal output";
  license = stdenv.lib.licenses.bsd3;
}
