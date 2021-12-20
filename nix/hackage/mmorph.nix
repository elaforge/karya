{ mkDerivation, base, mtl, lib, transformers
, transformers-compat
}:
mkDerivation {
  pname = "mmorph";
  version = "1.1.5";
  sha256 = "46fb450e3dedab419c47b0f154badb798c9e0e8cd097f78c40a12b47e1a8092f";
  revision = "1";
  editedCabalFile = "087v8ajcfpx4f0v4jxvv16h6jswgnkfnyfn28k406d5w3ihcx1wl";
  libraryHaskellDepends = [
    base mtl transformers transformers-compat
  ];
  description = "Monad morphisms";
  license = lib.licenses.bsd3;
}
