# See TODO nix: for current status.
# Run tools/cachix-push to update the cache.

# Examples:
# nix-shell --attr buildEnv --run '$setup'
# or tools/nix-enter
# nix build -L -f default.nix buildEnv

# Building lilypond drags in all of texlive.  It's also marked broken on
# darwin for 19.09.
{ withLilypond ? false
, withIm ? true # TODO sync this with Shake.Config.enableIm
, withEkg ? false # ekg is really heavy
, withDocs ? false # deps to build documentation
, useSystemCc ? false # if false use the nixpkgs c++ compiler
, useSystemSupercollider ? true # if false use one declared here

# Enable profiling in hackage libraries.  Generally we want this to be able to
# profile, but it's much faster to build without it.
, profiling ? true
# Whether to do auto-scc stuff in hackage.  I prefer manual SCCs since auto
# ones defeat inlining and change performance too much, but it can be useful
# to see into hackage libraries sometimes.
# https://cabal.readthedocs.io/en/3.4/cabal-project.html#cfg-field-profiling-detail
, profilingDetail ? "none"
, isCi ? false
}:

let
  nixpkgs-sys = import <nixpkgs> {};
  nixpkgs = import nix/nixpkgs.nix { inherit config; };
  nixpkgs-orig = import nix/nixpkgs.nix {};
  hackage = import nix/hackage.nix {
    inherit ghcVersion profiling profilingDetail;
  };
  faust = import nix/faust.nix {};
  inherit (nixpkgs) lib;

  ghcVersionDots = let ver = p: lib.hasPrefix p lib.version;
    in if ver "19.09" then "8.8.2" # ghc883 is not in 1909 yet
    # else if ver "20.09" then "8.8.4"
    else if ver "20.09" then "8.10.3"
    # else if ver "21.11" then "8.10.7"
    else if ver "21.11" then "9.2.1"
    else abort "no ghc version defined for nixpkgs version ${lib.version}";

  ghcVersion = "ghc" + builtins.replaceStrings ["."] [""] ghcVersionDots;
  ghc = nixpkgs.haskell.packages.${ghcVersion};

  # This should just hit the nixos cache.  The benefit is that it won't reuse
  # my pinned library versions, so I don't have to make sure they're
  # compatible.  The drawback is that it won't use my pinned library versions,
  # so lots of more downloading.
  haskellBinary = name: nixpkgs-orig.haskellPackages.${name};
  # Minimal compile just for build-time binary deps.
  sharedHaskellBinary = name: with nixpkgs.haskell.lib;
    dontCheck (disableLibraryProfiling
      (disableExecutableProfiling ghc."${name}"));

  config = {
    # For nixpkgs.mkl, for mesh2faust.
    allowUnfree = true;
    packageOverrides = pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${ghcVersion}" = pkgs.haskell.packages."${ghcVersion}".override {
            overrides = new: old: hackage.overrides old;
          };
        };
      };
    };
    # Not working for some reason.
    # overlays = [overlay];
  };

  # TODO: not used, couldn't get it to work
  _overlay = nixpkgsSelf: nixpkgsSuper:
    let
      inherit (nixpkgsSelf) pkgs;
      # pkgs = self.pkgs;
      hsPkgs = nixpkgsSuper.haskell.packages.${ghcVersion}.override {
        overrides = self: super: {
          Diff = pkgs.haskell.lib.dontCheck (
            self.callHackage "Diff" "0.4.0" {}
          );
        };
      };
    in {
      haskell = nixpkgsSuper.haskell // {
        packages = nixpkgsSuper.haskell.packages // {
          "${ghcVersion}" = hsPkgs;
        };
      };
    };

  # util
  guard = bool: list: if bool then list else [];
  split = sep: str:
    builtins.filter builtins.isString (builtins.split sep str);
  lines = str: builtins.filter (s: s != "") (split "\n" str);
  readLines = fn: lines (builtins.readFile fn);

  hsBool = b: if b then "True" else "False";

  inherit (nixpkgs.stdenv) isDarwin isLinux;
in rec {
  # Put some things in here for convenience from `nix repl default.nix`.
  inherit nixpkgs ghc hackage;
  inherit nixpkgs-orig;
  inherit nixpkgs-sys;
  inherit (hackage) nixFiles;

  # This may be desirable to get a consistent supercollider, especially
  # one with a consistent jack version.  But the default is to assume
  # there is already a system supercollider which works.
  supercollider = nixpkgs.libsForQt512.callPackage nix/supercollider.nix {
    fftw = nixpkgs.fftwSinglePrec;
    useIDE = false;
    # jack can't torelate any version skew, see libjack2 usage below.
    inherit (nixpkgs-sys) libjack2;
  };

  # nixpkgs.rubberband only works on linux.
  rubberband = if isDarwin
    then nixpkgs.callPackage nix/rubberband-darwin.nix {
        inherit (nixpkgs.darwin.apple_sdk.frameworks)
          Accelerate CoreGraphics CoreVideo;
      }
    else nixpkgs.rubberband;

  # I want some unreleased fixes, for mousewheel and Fl_Image_Surface.
  fltk =
    let
      commit = "84c09ae7b2de0ad9142551ebd4f53a7e113902b4";
      name = "fltk-1.4-${commit}";
      src = builtins.fetchGit {
        url = "https://github.com/fltk/fltk.git";
        rev = commit;
        ref = "master";
      };
    # It's 21.11 but winds up being 21.11pre-git which is considered < 21.11
    in if builtins.compareVersions lib.version "21.10" >= 0 then
      (nixpkgs.fltk14-minimal.override {
        withShared = false;
      }).overrideAttrs (old: { inherit name src; })
    else
      nixpkgs.fltk14.overrideAttrs (old: {
        inherit name src;
        nativeBuildInputs = [nixpkgs.autoconf] ++ old.nativeBuildInputs;
        # TODO The nixpkgs version has deps on GL, freetype, libtiff, which
        # I think I can omit.
        configureFlags = [];
        # dontUseCmakeConfigure = true; # only necessary for new cmake build
      });

  hackageGhc =
    let wantPkg = pkg:
        # nix gets the "ghc" package confused with the compiler.
        pkg != "ghc" ;
    in ghc.ghcWithPackages (pkgs: map (pkg: pkgs."${pkg}") (
      builtins.filter wantPkg (builtins.concatLists [
        (readLines doc/cabal/basic)
        (guard withIm (readLines doc/cabal/im))
        (guard withEkg ["ekg"])
      ])
    ));

  midiDeps = if isLinux then [
    # Make sure to compile against the system version of jack, not my pinned
    # nixpkgs one.  Jack apparently has no version control in the protocol,
    # so version mismatches show up as random "Unknown request" junk.
    nixpkgs-sys.libjack2
  ] else if isDarwin then (with nixpkgs.darwin.apple_sdk.frameworks; [
    Cocoa
    CoreAudio
    CoreFoundation
    CoreMIDI
  ]) else abort "not linux or darwin, don't know how to do midi";

  # You always need these deps.
  basicDeps = [
    # fltk has to be in basicDeps, so it gets in buildEnv buildInputs, so the
    # magic nix hook puts it in NIX_LDFLAGS, so the magic nix gcc wrapper puts
    # the -L flag on.
    fltk
    hackageGhc
    (sharedHaskellBinary "cpphs")
    midiDeps
    # Many scripts are in zsh, I can't be bothered to put ""s everywhere.
    nixpkgs.zsh
  ] ++ guard isCi [
    nixpkgs.coreutils # at least one test uses cat
  ] ++ guard (!useSystemCc) [
    nixpkgs.stdenv.cc
  ] ++ guard (!useSystemSupercollider) [
    supercollider
  ];

  fontDeps = with nixpkgs; [
    # I don't really use these, but there they are in case I do someday.
    # noto-fonts
    openlilylib-fonts.bravura
  ];

  # Dependencies to actually use karya.  CI can omit them.
  interactiveDeps = fontDeps ++ [
    nixpkgs.git

    # development deps
    # nixpkgs-orig.haskell.packages.ghc8104.fast-tags
    (haskellBinary "fast-tags")
    # (haskellBinary "weeder")
    (haskellBinary "profiterole")
    (haskellBinary "ghc-prof-flamegraph")
    (haskellBinary "hp2html")
    nixpkgs.ripgrep
    # TODO nixpkgs.cachix is too old, instead:
    # nix-env -iA cachix -f https://cachix.org/api/v1/install
  ];

  docDeps = [
    nixpkgs-orig.pandoc
  ];

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
    configureFlags = ["--enable-shared=no" "--enable-static=yes"];
  };

  imDeps = [
    faust.faust
    libsamplerate
    nixpkgs.libsndfile
    rubberband
    # This is a build dep, not a library dep.
    ghc.c2hs
  ] ++ guard isLinux [
    # TODO for some reason the nixpkgs depends on fftw, but doesn't put it in
    # nix-support/propagated-build-inputs, which means the insane nixpkgs hooks
    # magic doesn't get it into NIX_LDFLAGS, which means the gcc wrapper
    # doesn't get the -L flag for it.  See NOTE [nix-ldflags]
    nixpkgs.fftw
  ];

  mod_to_sexpr = nixpkgs.callPackage nix/mod_to_sexpr.nix {};

  #### aggregate deps

  deps = builtins.concatLists [
    basicDeps
    (guard (!isCi) interactiveDeps)
    (guard withDocs docDeps)
    (guard withIm imDeps)
    (guard withLilypond [nixpkgs.lilypond])
  ];

  generatedByNix = "-- Generated by nix, delete or this will be overwritten!";
  # Make a nix-shell that can run `mkmk` and `mk`.
  shakeConfig =
    # https://github.com/NixOS/nixpkgs/issues/24237
    let
      extraLinkFlags = if isDarwin then ''
        -- nixpkgs bug on darwin: https://github.com/NixOS/nixpkgs/issues/24237
        , extraLinkFlags = ["-F/System/Library/Frameworks"]
        ''
        else "";
      imLibs = if withIm then ''
          , libsamplerate = C.ExternalLibrary
              { C.libLink = ["${libsamplerate}/lib/libsamplerate.a"]
              , C.libCompile = ["-I${libsamplerate}/include"]
              }
          , rubberband = C.ExternalLibrary
              { C.libLink = ["-lrubberband"]
              , C.libCompile = ["-I${rubberband}/include"]
              }
        ''
        else "";
    in nixpkgs.writeText "ShakeConfig.hs" ''
      ${generatedByNix}
      {-# OPTIONS_GHC -Wno-unused-imports #-}
      module Local.ShakeConfig where
      import qualified Shake.C as C
      import Shake.Config

      localConfig = defaultConfig
          { enableEkg = ${hsBool withEkg}
          , enableIm = ${hsBool withIm}
          , fltkConfig = "${fltk}/bin/fltk-config"
          ${imLibs}
          ${extraLinkFlags}
          }
    '';

  buildEnv = nixpkgs.stdenv.mkDerivation {
    name = "buildEnv";
    builder = "${nixpkgs.bash}/bin/bash";

    # TODO I use this with nix-shell, which doesn't run the build, so this is a
    # placeholder that just keeps the deps as GC roots.  In the future this
    # could do a `mkmk && mk typecheck` or something, and run in CI.  It can't
    # do that because it doesn't have any of the srcs.  I'm not sure the best
    # way to get those, perhaps a builtins.filterSource on ./. for *.hs, and
    # then link those into the build dir?  This would make a non-incremental CI
    # though, which would be really inefficient, or maybe I can just cache the
    # build directory, and have shake use fingerprints instead of mtime.
    args = ["-eu" (builtins.toFile "buildEnv-builder.sh" ''
      $coreutils/bin/mkdir -p $out/nix-support
      echo "$buildInputs" >$out/nix-support/deps
    '')];
    # srcs = ... TODO
    coreutils = nixpkgs.coreutils;

    # These wind up in PATH in nix-shell, since it runs $stdenv/setup.
    buildInputs = deps;

    # The ghc generated by nix doesn't need this, but GHC API as run by seq
    # does.
    GHC_PACKAGE_PATH = "${hackageGhc}/lib/ghc-${ghcVersionDots}/package.conf.d";

    # tools/nix-enter will run this.
    shellHook = nixpkgs.writeScript "setup.sh" ''
      # Don't replace an explicit config.
      cfg=Local/ShakeConfig.hs
      if [[ ! -e $cfg || "$(head -1 $cfg)" = "${generatedByNix}" ]]; then
        if ! cmp --silent ${shakeConfig} $cfg; then
          echo Adding generated $cfg
          # I do cp instead of link so that shake, which uses mtimes, will
          # notice that it changes.  Shake shouldn't use mtimes, but
          # unfortunately ghci uses mtimes, so I have to configure shake to do
          # that too.
          cp -f ${shakeConfig} $cfg
        fi
      else
        echo "not replacing since not generated by nix: $cfg"
      fi
    '';
  };
}

# NOTE [nix-ldflags]
#
# $stdenv/setup is supposed to collect -L flags for depended-on libraries and
# stuff them into $NIX_LDFLAGS.  If you use the nixpkgs "wrapped" gcc, it will
# then inject that into its commandline.  How this works (I think):
#
# . nix-support/setup-hook is run for all deps.  gcc-wrapper has one that
# adds a ccWrapper_addCVars function to the set of "env hooks".
# binutil-wrapper adds bintoolsWrapper_addLDVars.
# . Each input, if it has nix-support/propagated-build-inputs, chases down
# all transitive deps, and runs all the hooks on them.
# . bintoolsWrapper_addLDVars just looks for a `lib` subdir and stuffs it in
# NIX_LDFLAGS.
# . Wrapped gcc sees that etc.
#
# There is apparently some other magic that puts things in
# propagated-build-inputs, and it sometimes doesn't work.
