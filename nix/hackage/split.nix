{ mkDerivation, base, QuickCheck, stdenv }:
mkDerivation {
  pname = "split";
  version = "0.2.3.4";
  sha256 = "271fe5104c9f40034aa9a1aad6269bcecc9454bc5a57c247e69e17de996c1f2a";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base QuickCheck ];
  description = "Combinator library for splitting lists";
  license = stdenv.lib.licenses.bsd3;
}
