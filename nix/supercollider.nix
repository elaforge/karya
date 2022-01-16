{ stdenv, lib, mkDerivation, fetchurl, cmake, pkgconfig, alsaLib
, libjack2, libsndfile, fftw, curl, gcc
, libXt, qtbase, qttools, qtwebengine
, readline, qtwebsockets, useSCEL ? false, emacs

, fetchgit
# If false, don't build the graphical IDE.  This will skip a lot of heavy QT
# dependencies.
, useIDE ? true
}:

let
  inherit (lib) optional optionals;
in

mkDerivation rec {
  pname = "supercollider";
  version = "3.11.0";

  src = fetchgit {
    url = "https://github.com/elaforge/supercollider.git";
    # rev = "5016df34bc81e0f7bdaba8813d4d381aa4aaea8a";
    # sha256 = "1ahgvhf27ggbxp6xi1xs1rjldm81f4gizfkw4hg6y0wvsf42nq35";
    rev = "4342154c5549278e5ebb53dd634e2204fbc75964";
    sha256 = "1rjnqnrqxc32l9zjlxccwdpxpk1k67c1jmlyknwwldx1dmz1x5fc";
    fetchSubmodules = true;
  };

  # src = fetchurl {
  #   url = "https://github.com/supercollider/supercollider/releases/download/Version-${version}/SuperCollider-${version}-Source.tar.bz2";
  #   sha256 = "0l5j7sqrjlm85ql91ybcrvdykfkkwfqd7w3m4llbymw720r2ln9p";
  # };

  hardeningDisable = [ "stackprotector" ];

  cmakeFlags = [
      "-DSC_WII=OFF"
      "-DSC_EL=${if useSCEL then "ON" else "OFF"}"
      "-DCMAKE_BUILD_TYPE=Release"
      "-DNATIVE=ON"
    ] ++ optional (!useIDE) "-DSC_QT=OFF";

  nativeBuildInputs = [ cmake pkgconfig ] ++ optional useIDE qttools;

  enableParallelBuilding = true;

  buildInputs = builtins.concatLists [
    [ gcc libjack2 libsndfile fftw curl readline libXt ]
    (optionals useIDE [ qtbase qtwebengine qtwebsockets ])
    (optional (!stdenv.isDarwin) alsaLib)
    (optional useSCEL emacs)
  ];

  meta = with lib; {
    description = "Programming language for real time audio synthesis";
    homepage = "https://supercollider.github.io";
    maintainers = with maintainers; [ mrmebelman ];
    license = licenses.gpl3;
    platforms = [ "x686-linux" "x86_64-linux" ];
  };
}
