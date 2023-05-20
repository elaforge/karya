{ mkDerivation, assoc, base, binary, deepseq
, foldable1-classes-compat, hashable, lib
}:
mkDerivation {
  pname = "these";
  version = "1.2";
  sha256 = "a8ed7174b7f790764360f48aab72fea382a6093ed369c7bc6881fae1abacd4b8";
  libraryHaskellDepends = [
    assoc base binary deepseq foldable1-classes-compat hashable
  ];
  doCheck = false;
  homepage = "https://github.com/haskellari/these";
  description = "An either-or-both data type";
  license = lib.licenses.bsd3;
}
