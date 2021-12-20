{ mkDerivation, base, HTTP, lib }:
mkDerivation {
  pname = "js-jquery";
  version = "3.3.1";
  sha256 = "e0e0681f0da1130ede4e03a051630ea439c458cb97216cdb01771ebdbe44069b";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base HTTP ];
  doCheck = false;
  homepage = "https://github.com/ndmitchell/js-jquery#readme";
  description = "Obtain minified jQuery code";
  license = lib.licenses.mit;
}
