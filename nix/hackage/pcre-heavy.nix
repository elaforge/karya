{ mkDerivation, base, base-compat, bytestring, doctest, Glob
, pcre-light, semigroups, lib, string-conversions
, template-haskell
}:
mkDerivation {
  pname = "pcre-heavy";
  version = "1.0.0.2";
  sha256 = "8a5cf697b7683127812450cef57d0d74ac5c1117ec80618d10509642f793cbd1";
  revision = "1";
  editedCabalFile = "14pprgwxkiaji3rqhsm0fv454wic6qxm7vy4a475yigadb1vz1ls";
  libraryHaskellDepends = [
    base base-compat bytestring pcre-light semigroups
    string-conversions template-haskell
  ];
  testHaskellDepends = [ base doctest Glob ];
  homepage = "https://github.com/myfreeweb/pcre-heavy";
  description = "A regexp (regex) library on top of pcre-light you can actually use";
  license = lib.licenses.publicDomain;
}
