{ mkDerivation, attoparsec, base, base-compat
, base-compat-batteries, base-orphans, base16-bytestring
, bytestring, containers, data-fix, deepseq, Diff, directory, dlist
, filepath, generic-deriving, ghc-prim, hashable, hashable-time
, integer-logarithms, primitive, QuickCheck, quickcheck-instances
, scientific, stdenv, strict, tagged, tasty, tasty-golden
, tasty-hunit, tasty-quickcheck, template-haskell, text
, th-abstraction, these, time, time-compat, unordered-containers
, uuid-types, vector
}:
mkDerivation {
  pname = "aeson";
  version = "1.5.6.0";
  sha256 = "0361c34be3d2ec945201f02501693436fbda10dcc549469481a084b2de22bfe8";
  revision = "1";
  editedCabalFile = "1y7ddmghsjblsxaj1wyif66wrw0vvp2dca5i7v9rqk33z1r6iryk";
  libraryHaskellDepends = [
    attoparsec base base-compat-batteries bytestring containers
    data-fix deepseq dlist ghc-prim hashable primitive scientific
    strict tagged template-haskell text th-abstraction these time
    time-compat unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    attoparsec base base-compat base-orphans base16-bytestring
    bytestring containers data-fix Diff directory dlist filepath
    generic-deriving ghc-prim hashable hashable-time integer-logarithms
    QuickCheck quickcheck-instances scientific strict tagged tasty
    tasty-golden tasty-hunit tasty-quickcheck template-haskell text
    these time time-compat unordered-containers uuid-types vector
  ];
  homepage = "https://github.com/haskell/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}
