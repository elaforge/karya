{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "js-dgtable";
  version = "0.5.2";
  sha256 = "e28dd65bee8083b17210134e22e01c6349dc33c3b7bd17705973cd014e9f20ac";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/ndmitchell/js-dgtable#readme";
  description = "Obtain minified jquery.dgtable code";
  license = stdenv.lib.licenses.mit;
}
