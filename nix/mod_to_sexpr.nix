# Use libxmp to convert mods to a sexpr format that I can then load in
# Cmd.Load.ModSexpr.  It's simpler to do it this way than try to bind to libxmp
# from haskell.
{ stdenv, fetchurl }:
let
  # Copied from nixpkgs, but: can compile on darwin, cp effects.h postInstall.
  libxmp = stdenv.mkDerivation rec {
    name = "libxmp-4.4.1";
    postInstall = "cp src/effects.h $out/include";

    meta = with stdenv.lib; {
      description = "Extended module player library";
      homepage    = "http://xmp.sourceforge.net/";
      longDescription = ''
        Libxmp is a library that renders module files to PCM data. It supports
        over 90 mainstream and obscure module formats including Protracker
        (MOD), Scream Tracker 3 (S3M), Fast Tracker II (XM), and Impulse
        Tracker (IT).
      '';
      license     = licenses.lgpl21Plus;
    };

    src = fetchurl {
      url = "mirror://sourceforge/xmp/libxmp/${name}.tar.gz";
      sha256 = "1kycz4jsyvmf7ny9227b497wc7y5ligydi6fvvldmkf8hk63ad9m";
    };
  };

in stdenv.mkDerivation {
  name = "mod_to_sexpr";
  src = ../Cmd/Load/mod_to_sexpr.c;
  phases = "buildPhase fixupPhase";
  buildPhase = ''
    mkdir -p $out/bin
    cc -o $out/bin/mod_to_sexpr $src -lxmp
  '';
  buildInputs = [libxmp];
}
