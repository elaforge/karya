# This is darwin only, since the nixpkgs version is linux-only.
{ stdenv, lib, fetchurl, pkgconfig
, Accelerate, CoreGraphics, CoreVideo
}:

stdenv.mkDerivation {
  name = "rubberband-2.0.0";

  src = fetchurl {
    url = "https://breakfastquay.com/files/releases/rubberband-2.0.0.tar.bz2";
    sha256 = "11h24vz3n035nkj10hw8srpafxk53v1ywg23la337klnaiag1jzc";
  };

  phases = "unpackPhase buildPhase installPhase";
  buildPhase = ''
    cp otherbuilds/Makefile.macos .
    substituteInPlace Makefile.macos --replace /usr/local "$out"
    mkdir -p lib
    make -f Makefile.macos static
  '';
  # They recommend meson to build, but the makefile is simpler and seems to
  # work for a static build.
  installPhase = ''
    mkdir -p $out/include/rubberband $out/lib
    cp rubberband/rubberband-c.h rubberband/RubberBandStretcher.h \
      $out/include/rubberband
    cp lib/*.a lib/*.dylib $out/lib
  '';

  buildInputs = [
    Accelerate CoreGraphics CoreVideo
  ];

  meta = {
    homepage = "https://www.breakfastquay.com/rubberband/index.html";
    license = lib.licenses.gpl2Plus;
  };
}
