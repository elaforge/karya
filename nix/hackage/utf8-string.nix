{ mkDerivation, base, bytestring, HUnit, lib }:
mkDerivation {
  pname = "utf8-string";
  version = "1.0.2";
  sha256 = "ee48deada7600370728c4156cb002441de770d0121ae33a68139a9ed9c19b09a";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [ base HUnit ];
  homepage = "https://github.com/glguy/utf8-string/";
  description = "Support for reading and writing UTF8 Strings";
  license = lib.licenses.bsd3;
}
