{ mkDerivation, base, stdenv, unix }:
mkDerivation {
  pname = "unix-compat";
  version = "0.5.2";
  sha256 = "659a4e442f71505d45b0d0fb28a347aa1ac5e1f39feaea5d58e674e5d2ce0ba9";
  revision = "1";
  editedCabalFile = "1yx38asvjaxxlfs8lpbq0dwd84ynhgi7hw91rn32i1hsmz7yn22m";
  libraryHaskellDepends = [ base unix ];
  homepage = "http://github.com/jacobstanley/unix-compat";
  description = "Portable POSIX-compatibility layer";
  license = stdenv.lib.licenses.bsd3;
}
