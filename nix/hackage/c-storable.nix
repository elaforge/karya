{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "c-storable";
  version = "0.2";
  sha256 = "32d5cf746d48b58ce4cb9f1017ed156fc170e79098dd052ed650d4654da5d60f";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/elaforge/c-storable";
  description = "CStorable class";
  license = stdenv.lib.licenses.bsd3;
}
