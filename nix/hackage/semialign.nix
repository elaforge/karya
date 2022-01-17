{ mkDerivation, base, containers, hashable, indexed-traversable
, indexed-traversable-instances, lib, semigroupoids, tagged, these
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "semialign";
  version = "1.2.0.1";
  sha256 = "d900697041ae4b0cca3243273a2b3e80bcf74d937405d6a5ff34dc33ee952132";
  revision = "1";
  editedCabalFile = "00cmkdmgqlk8v2jg084nddlj2qkwj68nqk9p3l07kzwf796rn5qf";
  libraryHaskellDepends = [
    base containers hashable indexed-traversable
    indexed-traversable-instances semigroupoids tagged these
    transformers unordered-containers vector
  ];
  doCheck = false;
  homepage = "https://github.com/haskellari/these";
  description = "Align and Zip type-classes from the common Semialign ancestor";
  license = lib.licenses.bsd3;
}
