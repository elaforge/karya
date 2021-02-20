{ mkDerivation, base, base16-bytestring, bytestring, criterion
, pureMD5, stdenv, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptohash-md5";
  version = "0.11.100.1";
  sha256 = "710bd48770fa3e9a3b05428c6dc77fb72c91956d334a1eb89ded11bb843e18f9";
  revision = "5";
  editedCabalFile = "1dxakry5p9dv9p9h617c0295plrfy2n5k13lsgp4gxwb6yzji9d4";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base base16-bytestring bytestring pureMD5 tasty tasty-hunit
    tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  homepage = "https://github.com/hvr/cryptohash-md5";
  description = "Fast, pure and practical MD5 implementation";
  license = stdenv.lib.licenses.bsd3;
}
