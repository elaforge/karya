{ mkDerivation, base, containers, criterion, indexed-traversable
, lib, OneTuple, QuickCheck, quickcheck-instances, tagged, tasty
, tasty-quickcheck, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "indexed-traversable-instances";
  version = "0.1.1";
  sha256 = "100ed1023b541328b04bcec0964b9f9d5fc93285fc23a2ac6873bf8597439a44";
  libraryHaskellDepends = [
    base indexed-traversable OneTuple tagged unordered-containers
    vector
  ];
  testHaskellDepends = [
    base containers indexed-traversable OneTuple QuickCheck
    quickcheck-instances tasty tasty-quickcheck transformers
    unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    base containers criterion indexed-traversable unordered-containers
    vector
  ];
  doCheck = false;
  description = "More instances of FunctorWithIndex, FoldableWithIndex, TraversableWithIndex";
  license = lib.licenses.bsd2;
}
