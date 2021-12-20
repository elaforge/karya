{ mkDerivation, base, bifunctors, lib, tagged }:
mkDerivation {
  pname = "assoc";
  version = "1.0.2";
  sha256 = "d8988dc6e8718c7a3456515b769c9336aeeec730cf86fc5175247969ff8f144f";
  revision = "1";
  editedCabalFile = "17ycclzwnysca80frsyyb6sdd2r5p83lkgwxjjnjg6j62pvf8958";
  libraryHaskellDepends = [ base bifunctors tagged ];
  description = "swap and assoc: Symmetric and Semigroupy Bifunctors";
  license = lib.licenses.bsd3;
}
