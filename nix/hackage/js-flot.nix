{ mkDerivation, base, HTTP, lib }:
mkDerivation {
  pname = "js-flot";
  version = "0.8.3";
  sha256 = "1ba2f2a6b8d85da76c41f526c98903cbb107f8642e506c072c1e7e3c20fe5e7a";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base HTTP ];
  homepage = "https://github.com/ndmitchell/js-flot#readme";
  description = "Obtain minified flot code";
  license = lib.licenses.mit;
}
