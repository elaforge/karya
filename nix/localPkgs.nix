# Expose my local pkgs definitions.
{ nixpkgs, nixpkgs-sys }:
let
  inherit (nixpkgs.stdenv) isDarwin isLinux;
  faustStuff = import ./faust.nix {};

  jacks = {
    # Make sure to compile against the system version of jack, not my pinned
    # nixpkgs one.  Jack apparently has no version control in the protocol, so
    # version mismatches show up as random "Unknown request" junk.  So I need
    # the same one as the system.  For nixos I can get it from <nixpkgs>, but
    # for other distros I'll probably need to have a list of versions and
    # manually pick the one that's the same as the system.
    nixos = nixpkgs-sys.libjack2;
    # Untested.
    v1_9_22 = nixpkgs.libjack2.overrideAttrs (old: {
      src = nixpkgs.fetchFromGitHub {
        owner = "jackaudio";
        repo = "jack2";
        rev = "v1.9.22";
        sha256 = "sha256-Cslfys5fcZDy0oee9/nM5Bd1+Cg4s/ayXjJJOSQCL4E=";
      };
      prePatch = "";
      # svnversion_regenerate.sh doesn't seem to exist.
      # postPatch = ''
      #   patchShebangs --build svnversion_regenerate.sh
      # '';
    });
  };
in rec {
  faust = faustStuff.faust;

  fltk =
    # Upgrade to 1.4.1 release.
    let
      src = nixpkgs.fetchFromGitHub {
        owner = "fltk";
        repo = "fltk";
        rev = "release-1.4.1";
        sha256 = "sha256-cm2jskrVrbYEJkGAb/s4Mh+et56//2+ypVEWNdqmhhE=";
      };
      name = "fltk-1.4.1";
    in if isDarwin then
      (nixpkgs.fltk14-minimal.override {
        withShared = false;
      }).overrideAttrs (old: {
        inherit name src;
        # Rejected Fl_cocoa.mm patch.
        patches = [];
        buildInputs = old.buildInputs ++ [
          # Otherwise:
          # > source/src/Fl_Native_File_Chooser_MAC.mm: 32:11: fatal error:
          # 'UniformTypeIdentifiers/UniformTypeIdentifiers.h' file not found
          nixpkgs.darwin.apple_sdk.frameworks.UniformTypeIdentifiers
        ];
        postInstall = "";
      })
    else
      (nixpkgs.fltk14-minimal.override {
        withShared = false;
      }).overrideAttrs (old: {
        inherit name src;
      })
    ;

  fltkOld =
    let
      # I want some unreleased fixes, for mousewheel and Fl_Image_Surface.
      commit = "84c09ae7b2de0ad9142551ebd4f53a7e113902b4";
      name = "fltk-1.4-${commit}";
      src = builtins.fetchGit {
        url = "https://github.com/fltk/fltk.git";
        rev = commit;
        ref = "master";
      };
    in
      (nixpkgs.fltk14-minimal.override {
        withShared = false;
      }).overrideAttrs (old: { inherit name src; });

  # TODO: for non-nixos, have a way to choose 1.9.22, assuming the system uses
  # that version?  I don't have any non-nixos linux, or reasons to test on
  # them, so I'll leave it here until such a system comes along.
  libjack2 = jacks.nixos;

  libsamplerate = nixpkgs.stdenv.mkDerivation {
    # libsamplerate with my patches to save and resume. The official one is
    # nixpkgs.libsamplerate.  I compile without libsndfile and none of the
    # utils, so deps on e.g. CoreServices are not needed.
    name = "libsamplerate-elaforge";
    src = builtins.fetchGit {
      url = "https://github.com/elaforge/libsamplerate.git";
      rev = "cb783007e114531911ec4f2f081a27733c84b45c";
      ref = "save-state";
    };
    nativeBuildInputs = with nixpkgs; [autoreconfHook pkgconfig];
    # I use the static version, but ghci will require the dynamic one.
    # I try to avoid having the dep at all, but in case I do, here it is.
    configureFlags = ["--enable-shared=yes" "--enable-static=yes"];
  };

  # nixpkgs.rubberband only works on linux.
  rubberband = if isDarwin
    then nixpkgs.callPackage ./rubberband-darwin.nix {
        inherit (nixpkgs.darwin.apple_sdk.frameworks)
          Accelerate CoreGraphics CoreVideo;
      }
    else nixpkgs.rubberband;

  # This may be desirable to get a consistent supercollider, especially
  # one with a consistent jack version.  But the default is to assume
  # there is already a system supercollider which works.
  supercollider = nixpkgs.libsForQt512.callPackage ./supercollider.nix {
    fftw = nixpkgs.fftwSinglePrec;
    useIDE = false;
    inherit libjack2;
  };
}
