# nix build -L -f nix/faust.nix faust
{ nixpkgs ? import ./nixpkgs.nix {} }:
let
  stdenv = nixpkgs.stdenv;
  llvm = nixpkgs.llvm_5;

  faustSrc = builtins.fetchGit {
    url = "git@github.com:grame-cncm/faust.git";
    rev = "fbd6e12c81832fed47606796dc9879b695adfa2f"; # 2019-11-18
    ref = "master-dev";
  };
  # This is also available in the main faust repo, but as a submodule.
  faustLib = builtins.fetchGit {
    url = "git@github.com:grame-cncm/faustlibraries.git";
    rev = "127e5bfcb35dc1d416d5f83ff0cd1fe38d458800";
    ref = "master";
  };
in {
  faust = stdenv.mkDerivation {
    name = "faust";
    src = faustSrc;

    nativeBuildInputs = with nixpkgs; [
        cmake
        pkgconfig
    ];
    buildInputs = with nixpkgs; [
        llvm
    ];
    phases = "unpackPhase buildPhase installPhase";
    buildPhase = "make";
    faustLib = faustLib.outPath;
    installPhase = ''
      mkdir $out
      PREFIX=$out make install
      # I don't know why, but install puts architecture files
      # in share/faust, but looks for them in architecture, in addition to
      # share/faust.  But it only looks for the stdlib in share/faust.
      mv $out/share/faust $out/architecture
      cp -R $faustLib $out/share/faust
    '';
  };

  mesh2faust = stdenv.mkDerivation {
    name = "mesh2faust";
    src = faustSrc;
    preBuild = ''
      cd tools/physicalModeling/mesh2faust
      header=vega/Makefile-headers/Makefile-header
      rm -f $header
      cp ${if stdenv.isDarwin then ./vega-makefile-header.osx
        else ./vega-makefile-header.linux} $header
    '';
    # 'make install' copies to /usr/local/bin.
    installPhase = ''
      mkdir -p $out/bin
      cp src/mesh2faust $out/bin
    '';
    # The nixpkgs one makes segfaulting binaries.
    # arpack = nixpkgs.arpack;
    arpack = /usr/local/Cellar/arpack/3.7.0_3/libexec;
    mkl = nixpkgs.mkl;
    buildInputs = with nixpkgs; [
      zlib
    ] ++ (if stdenv.isDarwin then with nixpkgs.darwin.apple_sdk.frameworks; [
      Accelerate
      CoreGraphics
      CoreVideo
      Foundation
      GLUT
      OpenGL
    ] else with nixpkgs; [
      freeglut
      libGLU
      mesa
    ]);
  };
}
