# I have nixpkgs-sys, solely so I can get the system libjack2, jack protocol is
# version specific, so I need to use the system version instead of the one from
# my pinned nixpkgs.
#
# First thing is want a devShell.
#
# It puts the whole repo into /nix/store, but I just need the nix files.
# I'm not usually building in nix, just making the dev shell.
#
# - pass nixpkgs to hackage.nix, but it adds a new config.  I guess it becomes
# a differently configured nixpkgs.
# - all-cabal-hashes probably must become an input.  But it's not a flake, no
# flake.nix, so maybe not.
{
  description = "karya";
  inputs = {
    # This must be all literals.
    # https://github.com/NixOS/nix/issues/4945
    # https://github.com/NixOS/nix/issues/3966
    baseNixpkgs.url =
      "github:NixOS/nixpkgs/4d2b37a84fad1091b9de401eb450aae66f1a741e"; # 22.11
    # all-cabal-hashes.url =
    #   "github:commercialhaskell/all-cabal-hashes/772e77626d059b5df478cd33fff22a315fa0e0c5";
  };
  outputs = { self, baseNixpkgs }:
    let
      # TODO next use flake-utils.lib.eachDefaultSystem (system: ...)
      system = "aarch64-darwin";
      # TODO probably endswith "-darwin", what does stdenv.isDarwin do?
      isDarwin = system == "aarch64-darwin";
      isLinux = !isDarwin;

      # https://discourse.nixos.org/t/using-nixpkgs-legacypackages-system-vs-import/17462
      # This is the better way if there are downstream flakes:
      # nixpkgs = baseNixpkgs.legacyPackages.${system};
      # But it can't be configured, and there are no downstream flakes, so:
      nixpkgs = import baseNixpkgs {
        inherit system;
        config = {
          packageOverrides = pkgs: {
            haskell = pkgs.haskell // {
              packages = pkgs.haskell.packages // {
                "${ghcVersion}" = pkgs.haskell.packages.${ghcVersion}.override {
                  overrides = new: old: hackage.overrides old;
                };
              };
            };
          };
        };
      };
      # In default.nix:
      # nixpkgs-sys = if isCi then nixpkgs else import <nixpkgs> {};
      # But I don't think we have access to system <nixpkgs> in flakes.
      nixpkgs-sys = nixpkgs;
      hackage = import ./hackage.nix {
        inherit ghcVersion profilingDetail;
        inherit profiling;
        # I could do this to reduce builds in CI, but then it makes it miss the
        # cache I use for development.
        # profiling = if isCi then false else profiling;
      };
      localPkgs = import ./localPkgs.nix {
        inherit nixpkgs nixpkgs-sys system;
      };

      # The below is mostly copy pasted from default.nix.

      # These are the arguments to tools/nix-enter, but flakes can't be
      # parameterized yet.
      isCi = false;
      useSystemCc = false;
      useSystemSupercollider = true;
      withDevelopment = false;
      withEkg = false; # ekg is really heavy
      profilingDetail = "none";
      profiling = true;

      ### util

      guard = bool: list: if bool then list else [];
      split = sep: str:
        builtins.filter builtins.isString (builtins.split sep str);
      lines = str: builtins.filter (s: s != "") (split "\n" str);
      readLines = fn: lines (builtins.readFile fn);

      ### deps

      inherit (localPkgs) faust fltk libjack2 libsamplerate rubberband
        supercollider;

      # TODO: how to do the hackage overrides in flakes?  E.g. flake version of
      ghcVersionDots = "9.2.5";
      ghcVersion = "ghc" + builtins.replaceStrings ["."] [""] ghcVersionDots;
      ghc = nixpkgs.haskell.packages.${ghcVersion};
      hackageGhc =
        let wantPkg = pkg:
            # nix gets the "ghc" package confused with the compiler.
            pkg != "ghc" ;
        in ghc.ghcWithPackages (pkgs: map (pkg: pkgs."${pkg}") (
          builtins.filter wantPkg (builtins.concatLists [
            (readLines ../doc/cabal/nix-packages)
            (guard withEkg ["ekg"])
          ])
        ));

      midiDeps = if isLinux then [
        # Make sure to compile against the system version of jack, not my
        # pinned nixpkgs one.  Jack apparently has no version control in the
        # protocol, so version mismatches show up as random "Unknown request"
        # junk.
        libjack2
      ] else if isDarwin then (with nixpkgs.darwin.apple_sdk.frameworks; [
        Cocoa
        CoreAudio
        CoreFoundation
        CoreMIDI
      ]) else abort "not linux or darwin, don't know how to do midi";

      # You always need these deps.
      basicDeps = [
        # fltk has to be in basicDeps, so it gets in buildEnv buildInputs, so
        # the magic nix hook puts it in NIX_LDFLAGS, so the magic nix gcc
        # wrapper puts the -L flag on.
        fltk
        # TODO: ./hackage.nix needs to take this nixpkgs
        # hackageGhc
        # (sharedHaskellBinary "cpphs")
        # midiDeps
        # # Many scripts are in zsh, I can't be bothered to put ""s everywhere.
        # nixpkgs.zsh
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
      ];

      developmentDeps = [
        # TODO
        # # (sharedHaskellBinary "cabal-install") # needed for nix/c2n
        # # These haskell binaries will pull in a whole new ghc compiler because
        # # of the nixpkgs bug with dynamic linking.
        # (haskellBinary "fast-tags")
        # # (haskellBinary "weeder")
        # (haskellBinary "profiterole")
        # (haskellBinary "ghc-prof-flamegraph")
        # (haskellBinary "hp2html")
        # nixpkgs.ripgrep
      ];

      imDeps = with localPkgs; [
        # faust # TODO: currentSystem missing
        libsamplerate
        nixpkgs.libsndfile
        nixpkgs.flac.dev nixpkgs.flac # libflac 1.3.3
        rubberband
        # This is a build dep, not a library dep.
        # ghc.c2hs # TODO ghc
      ];
    in {
      # `nix repl nix/.` to inspect pkgs:
      # inherit pkgs;
      devShell.aarch64-darwin = nixpkgs.mkShell {
        buildInputs = builtins.concatLists [
          basicDeps
          imDeps
          (guard (!isCi) interactiveDeps)
          (guard withDevelopment developmentDeps)
          # TODO
          # (guard withDocs docDeps)
          # (guard withLilypond [nixpkgs.lilypond])
        ];
        # what does this do?
        inputsFrom = [];
      };
    };
}
