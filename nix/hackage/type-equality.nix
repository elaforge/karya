{ mkDerivation, base, lib }:
mkDerivation {
  pname = "type-equality";
  version = "1";
  sha256 = "4728b502a211454ef682a10d7a3e817c22d06ba509df114bb267ef9d43a08ce8";
  revision = "2";
  editedCabalFile = "1a3irpv5kyg3rywhmcp5fwg5irrdbdr0hrlw7asdk113nakrba7j";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/hesselink/type-equality";
  description = "Data.Type.Equality compat package";
  license = lib.licenses.bsd3;
}
