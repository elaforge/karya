{ mkDerivation, base, base-orphans, containers, hashable
, indexed-traversable, indexed-traversable-instances, lib
, QuickCheck, quickcheck-instances, tasty, tasty-quickcheck
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "witherable";
  version = "0.4.2";
  sha256 = "790d2bb274283419173bd89104439860675a9410f70f21912973ecd9098b4104";
  revision = "2";
  editedCabalFile = "1ljnv5xf6w7x58akj0a0yw16j63jkka0dvfvmjqwbn76aqg3pzc1";
  libraryHaskellDepends = [
    base base-orphans containers hashable indexed-traversable
    indexed-traversable-instances transformers unordered-containers
    vector
  ];
  testHaskellDepends = [
    base containers hashable QuickCheck quickcheck-instances tasty
    tasty-quickcheck transformers unordered-containers vector
  ];
  doCheck = false;
  homepage = "https://github.com/fumieval/witherable";
  description = "filterable traversable";
  license = lib.licenses.bsd3;
}
