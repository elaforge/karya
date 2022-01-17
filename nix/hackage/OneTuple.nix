{ mkDerivation, base, base-orphans, hashable, lib, template-haskell
}:
mkDerivation {
  pname = "OneTuple";
  version = "0.3.1";
  sha256 = "98853682d52fb4cc37a45cd186fbd77cf2565d3df5171acc4cf026427e103eef";
  revision = "2";
  editedCabalFile = "0gk0656igxl0km9kgh8v7b5vq74kla59ka9hvpzq57njr6bc0j58";
  libraryHaskellDepends = [
    base base-orphans hashable template-haskell
  ];
  testHaskellDepends = [ base hashable template-haskell ];
  doCheck = false;
  description = "Singleton Tuple";
  license = lib.licenses.bsd3;
}
