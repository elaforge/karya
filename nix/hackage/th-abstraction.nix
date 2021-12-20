{ mkDerivation, base, containers, ghc-prim, lib
, template-haskell
}:
mkDerivation {
  pname = "th-abstraction";
  version = "0.4.2.0";
  sha256 = "ea06b2cda25fc4b52dac48cc23e5a756f997df8985ecaee5a554202508a11c40";
  revision = "1";
  editedCabalFile = "1yc17r29vkwi4qzbrxy1d3gra87hk3ghy1jzfmrl2q8zjc0v59vb";
  libraryHaskellDepends = [
    base containers ghc-prim template-haskell
  ];
  testHaskellDepends = [ base containers template-haskell ];
  homepage = "https://github.com/glguy/th-abstraction";
  description = "Nicer interface for reified information about data types";
  license = lib.licenses.isc;
}
