# nix-store -r $(nix-instantiate faust.nix -A faust)
# nix build -f nix/faust.nix faust
{ nixpkgs ? import <nixpkgs> {} }:
let
  llvm = nixpkgs.llvm_5;
  faustSrc = nixpkgs.fetchFromGitHub {
      owner = "grame-cncm";
      repo = "faust";
      rev = "094b9c08b5708c908b3396293c3512bf925966f1";
      sha256 = "1pci8ac6sqrm3mb3yikmmr3iy35g3nj4iihazif1amqkbdz719rc";
      fetchSubmodules = true;
  };
in {
  faust = nixpkgs.stdenv.mkDerivation {
    name = "faust";
    src = faustSrc;

    # New version of faust.

    # The new version seems to have switched from make to cmake, so the old
    # stuff won't work.

    # # Version set to 2.18.0
    # src = builtins.fetchGit {
    #     url = "git@github.com:grame-cncm/faust.git";
    #     rev = "094b9c08b5708c908b3396293c3512bf925966f1";
    #     ref = "master-dev";
    # };
    #
    # nativeBuildInputs = with nixpkgs; [
    #     cmake
    #     pkgconfig
    # ];
    # buildInputs = with nixpkgs; [
    #     llvm
    # ];
    # # build should be:
    # # cd build; make
    # # But have to have llvm-config in the path
    #
    # # light - only compile c/c++ backend
    # preConfigure = ''
    #     # makeFlags="$makeFlags prefix=$out light"
    #     makeFlags="$makeFlags prefix=$out LLVM_CONFIG='${llvm}/bin/llvm-config' light"
    #     unset system
    # '';

    # Old version of faust.

    # This is a simplified copy of the nixpkgs faust derivation.  Omitting the
    # extra deps means it compiles on OS X.

    nativeBuildInputs = [ nixpkgs.pkgconfig ];
    buildInputs = [ llvm ];

    # Differences from the nixpkgs: -j6, target 'light' instead of 'world',
    # which is probably why I don't need so many deps.
    # Surely there's some way to get build cores en an env var?
    # ${NIX_BUILD_CORES}
    preConfigure = ''
      makeFlags="$makeFlags -j6 prefix=$out LLVM_CONFIG='${llvm}/bin/llvm-config' light"
      unset system
      sed '52iLLVM_VERSION=${nixpkgs.stdenv.lib.getVersion llvm}' -i compiler/Makefile.unix
    '';

    postPatch = ''
      # fix build with llvm 5.0.2 by adding it to the list of known versions
      # TODO: check if still needed on next update
      substituteInPlace compiler/Makefile.unix \
        --replace "5.0.0 5.0.1" "5.0.0 5.0.1 5.0.2"
    '';
  };

  mesh2faust = nixpkgs.stdenv.mkDerivation {
    name = "mesh2faust";
    src = faustSrc;
    # TODO I can use stdenv.isDarwin for linux/darwin choices.
    preBuild = ''
      cd tools/physicalModeling/mesh2faust
      header=vega/Makefile-headers/Makefile-header
      rm -f $header
      cp ${./vega-makefile-header.osx} $header
    '';
    # Disable 'make install', since it copies to /usr/local/bin.
    dontInstall = true;
    postBuild = ''
      mkdir -p $out/bin
      cp src/mesh2faust $out/bin
    '';
    arpack = nixpkgs.arpack;
    mkl = nixpkgs.mkl;
    buildInputs = with nixpkgs; [
      zlib
    ] ++ (with nixpkgs.darwin.apple_sdk.frameworks; [
      Accelerate
      CoreGraphics
      CoreVideo
      Foundation
      GLUT
      OpenGL
    ]);
  };
}
