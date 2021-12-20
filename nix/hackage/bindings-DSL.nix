{ mkDerivation, base, lib }:
mkDerivation {
  pname = "bindings-DSL";
  version = "1.0.25";
  sha256 = "63de32380c68d1cc5e9c7b3622d67832c786da21163ba0c8a4835e6dd169194f";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/jwiegley/bindings-dsl/wiki";
  description = "FFI domain specific language, on top of hsc2hs";
  license = lib.licenses.bsd3;
}
