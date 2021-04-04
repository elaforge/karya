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
, withDocs ? false # build documentation
, profiling ? false # enable profiling in dependent libraries
, isCi ? false
}:

let
  nixpkgs = import nix/nixpkgs.nix { inherit config; };
  nixpkgs-orig = import nix/nixpkgs.nix {};
  hackage = import nix/hackage.nix { inherit ghcVersion profiling; };
  faust = import nix/faust.nix {};
  inherit (nixpkgs) lib;

  ghcVersionDots = let ver = p: lib.hasPrefix p lib.version;
    in if ver "19.09" then "8.8.2" # ghc883 is not in 1909 yet
    # else if ver "20.09" then "8.8.4"
    else if ver "20.09" then "8.10.3"
    else abort "unknown version ${lib.version}";

  ghcVersion = "ghc" + builtins.replaceStrings ["."] [""] ghcVersionDots;
  getGhc = nixpkgs: nixpkgs.haskell.packages."${ghcVersion}";

  ghc = getGhc nixpkgs;
  ghc-orig = getGhc nixpkgs-orig;

  # This should just hit the nixos cache.  The benefit is that it won't reuse
  # my pinned library versions, so I don't have to make sure they're
  # compatible.  The drawback is that it won't use my pinned library versions,
  # so lots of more downloading.
  haskellBinary = name: ghc-orig."${name}";
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
  overlay = nixpkgsSelf: nixpkgsSuper:
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
  inherit nixpkgs ghc hackage;

  # nixpkgs.rubberband only works on linux.
  rubberband = if isDarwin
    then nixpkgs.callPackage nix/rubberband-darwin.nix {
        inherit (nixpkgs.darwin.apple_sdk.frameworks)
          Accelerate CoreGraphics CoreVideo;
      }
    else nixpkgs.rubberband;

  # I want some unreleased fixes, for mousewheel and Fl_Image_Surface.
  fltk =
    let commit = "3bb3429670f29bbf82424ce234d914201eee2d3f";
    in nixpkgs.fltk14.overrideDerivation (old: {
      name = "fltk-1.4-${commit}";
      src = builtins.fetchGit {
        url = "https://github.com/fltk/fltk.git";
        rev = commit;
        ref = "master";
      };
      # buildPhase = ''
      #   substituteInPlace makeinclude --replace .SILENT "# noisy"
      #   make -j$NIX_BUILD_CORES
      # '';
      nativeBuildInputs = [nixpkgs.autoconf] ++ old.nativeBuildInputs;

      # TODO The nixpkgs version has deps on GL, freetype, libtiff, which
      # I think I can omit.
      configureFlags = [];
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
    nixpkgs.libjack2
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
    (sharedHaskellBinary "fast-tags")
    (haskellBinary "weeder")
    nixpkgs.ripgrep
    # TODO nixpkgs.cachix is too old, instead:
    # nix-env -iA cachix -f https://cachix.org/api/v1/install
  ];

  docDeps = [
    (haskellBinary "hscolour")
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

  # Make a nix-shell that can run `mkmk` and `mk`.
  shakeConfig =
    # https://github.com/NixOS/nixpkgs/issues/24237
    let extraLinkFlags = if isDarwin
      then ''
        -- nixpkgs bug on darwin: https://github.com/NixOS/nixpkgs/issues/24237
        , extraLinkFlags = ["-F/System/Library/Frameworks"]
      ''
      else "";
    in nixpkgs.writeText "ShakeConfig.hs" ''
      -- Generated by nix
      module Local.ShakeConfig where
      import qualified Shake.C as C
      import Shake.Config

      localConfig = defaultConfig
          { enableEkg = ${hsBool withEkg}
          , enableIm = ${hsBool withIm}
          , libsamplerate = C.ExternalLibrary
              { C.libLink = ["${libsamplerate}/lib/libsamplerate.a"]
              , C.libCompile = ["-I${libsamplerate}/include"]
              }
          , rubberband = C.ExternalLibrary
              { C.libLink = ["-lrubberband"]
              , C.libCompile = ["-I${rubberband}/include"]
              }
          , fltkConfig = "${fltk}/bin/fltk-config"
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
      if [[ ! -e $cfg || "$(head -1 $cfg)" = "-- Generated by nix" ]]; then
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
