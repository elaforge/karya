{ mkDerivation, base, Cabal, directory, stdenv }:
mkDerivation {
  pname = "ghc-paths";
  version = "0.1.0.12";
  sha256 = "6ecbe676d073cb07989c61ce4c5709c4e67cbefdd2d55a4095f9388b6fe2c484";
  revision = "1";
  editedCabalFile = "1gb4hn87a78j1c2y1adi81y03irzkaxywscjkphfajsxc7f0ydw5";
  setupHaskellDepends = [ base Cabal directory ];
  libraryHaskellDepends = [ base ];
  description = "Knowledge of GHC's installation directories";
  license = stdenv.lib.licenses.bsd3;
}
