{ mkDerivation, base, lib }:
mkDerivation {
  pname = "generically";
  version = "0.1.1";
  sha256 = "04c5a436bec4b041f71a733f56a1bd7f435f63dde8d3eb5c1f48d55b4dbc43cf";
  revision = "1";
  editedCabalFile = "0pkyhym7q9v03pplpfjg80vmpk0cbgc56panfx9vcbzadvxmx6rb";
  libraryHaskellDepends = [ base ];
  doCheck = false;
  description = "Generically newtype to use with DerivingVia";
  license = lib.licenses.bsd3;
}
