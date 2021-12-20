{ mkDerivation, base, bindings-DSL, git, openssl, process, lib
, zlib
}:
mkDerivation {
  pname = "hlibgit2";
  version = "0.18.0.16";
  sha256 = "199e4027faafe0a39d18ca3168923d44c57b386b960c72398df1c0fb7eff8e5e";
  libraryHaskellDepends = [ base bindings-DSL zlib ];
  librarySystemDepends = [ openssl ];
  testHaskellDepends = [ base process ];
  testToolDepends = [ git ];
  description = "Low-level bindings to libgit2";
  license = lib.licenses.mit;
}
