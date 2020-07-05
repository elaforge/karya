# This is darwin only, since the nixpkgs version is linux-only.
{ stdenv, fetchurl, pkgconfig
, Accelerate, CoreGraphics, CoreVideo
}:

stdenv.mkDerivation {
  name = "rubberband-1.8.2";

  src = fetchurl {
    url = "https://breakfastquay.com/files/releases/rubberband-1.8.2.tar.bz2";
    sha256 = "1jn3ys16g4rz8j3yyj5np589lly0zhs3dr9asd0l9dhmf5mx1gl6";
  };

  phases = "unpackPhase buildPhase installPhase";
  buildPhase = ''
    substituteInPlace Makefile.osx --replace /usr/local "$out"
    mkdir -p lib
    make -f Makefile.osx static dynamic
  '';
  installPhase = ''
    mkdir -p $out/include/rubberband $out/lib
    cp rubberband/rubberband-c.h rubberband/RubberBandStretcher.h \
      $out/include/rubberband
    cp lib/*.a lib/*.dylib $out/lib
  '';

  buildInputs = [
    Accelerate CoreGraphics CoreVideo
  ];

  meta = with stdenv.lib; {
    homepage = "https://www.breakfastquay.com/rubberband/index.html";
    license = licenses.gpl2Plus;
  };
}
