{ mkDerivation, base, lib }:
mkDerivation {
  pname = "heaps";
  version = "0.4";
  sha256 = "89329df8b95ae99ef272e41e7a2d0fe2f1bb7eacfcc34bc01664414b33067cfd";
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/ekmett/heaps/";
  description = "Asymptotically optimal Brodal/Okasaki heaps";
  license = lib.licenses.bsd3;
}
