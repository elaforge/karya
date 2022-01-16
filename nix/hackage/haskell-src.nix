{ mkDerivation, array, base, happy, lib, pretty, syb }:
mkDerivation {
  pname = "haskell-src";
  version = "1.0.3.1";
  sha256 = "869cc710004c2161470d8a788dab96d2cff054fa106c301be6689109f57e5132";
  revision = "5";
  editedCabalFile = "1qaibp1b1szb3ci5lhsxa3lh7iwyfzr5gjnmb4nypqwjqs05dk2c";
  libraryHaskellDepends = [ array base pretty syb ];
  libraryToolDepends = [ happy ];
  description = "Support for manipulating Haskell source code";
  license = lib.licenses.bsd3;
}
