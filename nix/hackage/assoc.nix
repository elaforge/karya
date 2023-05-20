{ mkDerivation, base, lib, tagged }:
mkDerivation {
  pname = "assoc";
  version = "1.1";
  sha256 = "7aa2e6548b3d9d49a286ac20639479aaf6c47a1446113ed784d98737c5f60df4";
  libraryHaskellDepends = [ base tagged ];
  doCheck = false;
  description = "swap and assoc: Symmetric and Semigroupy Bifunctors";
  license = lib.licenses.bsd3;
}
