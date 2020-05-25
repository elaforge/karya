# Incomplete nix to gather all the dependencies.
# See TODO nix: for current status.
# https://www.srid.ca/haskell-nix.html

# Examples:
# nix-shell --attr buildEnv --run '$setup'
# or tools/nix-enter
# nix build -L -f default.nix --arg withIm true --arg withDocs true buildEnv

# Building lilypond drags in all of texlive.  It's also marked broken on
# darwin for 19.09.
{ withLilypond ? false
, withIm ? false # TODO sync this with Shake.Config.enableIm
, withEkg ? false # ekg is really heavy
, withDocs ? false
, profiling ? true # enable profiling in dependent libraries
}:

let
  nixpkgs = import nix/nixpkgs.nix { inherit config; };

  ghcVersion = "ghc882"; # ghc883 is not in 1909 yet

  ghc = nixpkgs.haskell.packages."${ghcVersion}";

  hackageSrcs = import nix/hackage.nix { inherit nixpkgs; };

  config = let
    # This should work better than jailbreak-cabal, but apparently needs
    # brand-new Cabal (3.0.1.0 lacks it, 3.2.0.0 has it).
    # jailbreak = drv: nixpkgs.haskell.lib.appendConfigureFlags drv
    #   ["--allow-older" "--allow-newer"];

    patchHackage = old: {
      # Otherwise zlib is circular because nixpkgs doesn't differentiate
      # haskell and C deps.
      zlib = { inherit (nixpkgs) zlib; };
      # Apparently missing this dep.
      digest = { inherit (nixpkgs) zlib; };
      hlibgit2 = {
        mkDerivation = args: old.mkDerivation (args // {
          # New gcc doesn't like -Wno-format without -Wno-format-security.
          patches = [nix/hlibgit2.patch];
        });
      };
    };
    overrideHackage = old:
      let
        toDrv = name: src:
          let
            drv = old.callCabal2nix name src
              ((patchHackage old)."${name}" or {});
          in with nixpkgs.haskell.lib;
            (if profiling then (x: x) else disableLibraryProfiling)
            (dontCheck drv);
      in builtins.mapAttrs toDrv hackageSrcs;

  in {
    # For nixpkgs.mkl, for mesh2faust.
    allowUnfree = true;

    packageOverrides = pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${ghcVersion}" = pkgs.haskell.packages."${ghcVersion}".override {
            overrides = new: old: overrideHackage old;
          };
        };
      };
    };
    # Not working for some reason.
    # overlays = [overlay];
  };

  # overlay = nixpkgsSelf: nixpkgsSuper:
  #   let
  #     inherit (nixpkgsSelf) pkgs;
  #     # pkgs = self.pkgs;
  #     hsPkgs = nixpkgsSuper.haskell.packages.${ghcVersion}.override {
  #       overrides = self: super: {
  #         Diff = pkgs.haskell.lib.dontCheck (
  #           self.callHackage "Diff" "0.4.0" {}
  #         );
  #       };
  #     };
  #   in {
  #     haskell = nixpkgsSuper.haskell // {
  #       packages = nixpkgsSuper.haskell.packages // {
  #         "${ghcVersion}" = hsPkgs;
  #       };
  #     };
  #   };

  # util
  guard = bool: list: if bool then list else [];
  split = sep: str:
    builtins.filter builtins.isString (builtins.split sep str);
  lines = str: builtins.filter (s: s != "") (split "\n" str);
  readLines = fn: lines (builtins.readFile fn);

  hsBool = b: if b then "True" else "False";
in rec {
  basicDeps = with nixpkgs; [
    fltk
    git
    hackage
    zsh
  ];

  fontDeps = with nixpkgs; [
    noto-fonts
    openlilylib-fonts.bravura
  ];

  docDeps = [
    ghc.hscolour
    nixpkgs.pandoc
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

  faust = import nix/faust.nix { inherit nixpkgs; };

  imDeps = [
    faust.faust
    libsamplerate
    nixpkgs.libsndfile
    nixpkgs.liblo
    # This is a build dep, not a library dep.
    ghc.c2hs
  ];

  wantPkg = pkg:
    # nix gets the "ghc" package confused with the compiler.
    pkg != "ghc"
    ;

  hackage = ghc.ghcWithPackages (pkgs: map (pkg: pkgs."${pkg}") (
    builtins.filter wantPkg (builtins.concatLists [
      (readLines doc/cabal/basic)
      (guard withIm (readLines doc/cabal/im))
      (guard withEkg ["ekg"])
    ])
  ));

  # hackage = ghc.ghcWithPackages (pkgs: with pkgs; [
  #   Diff MonadRandom QuickCheck
  #   tagged
  # ]);

  deps = builtins.concatLists [
    basicDeps
    fontDeps
    (guard withDocs docDeps)
    (guard withIm imDeps)
    (guard withLilypond [nixpkgs.lilypond])
  ];

  # Make a nix-shell that can run `mkmk` and `mk`.
  shakeConfig = nixpkgs.writeText "ShakeConfig.hs" ''
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
        }
  '';

  buildEnv = nixpkgs.stdenv.mkDerivation {
    name = "buildEnv";
    builder = "${nixpkgs.bash}/bin/bash";
    args = ["-eu" (builtins.toFile "buildEnv-builder.sh" ''
      # TODO surely I can use a standard builder script instead.
      PATH=""
      for p in $buildInputs; do
        export PATH=$p/bin''${PATH:+:}$PATH
      done

      mkdir $out
      cd $out
      for src in $srcs; do
        ln -s $src $(basename $src)
      done
    '')];
    srcs = deps;
    # These wind up in PATH in nix-shell, probably built-in nix-shell magic.
    buildInputs = [nixpkgs.coreutils] ++ deps;

    # tools/nix-enter will run this.
    setup = nixpkgs.writeScript "setup.sh" ''
      # Don't replace an explicit config.
      if [[ ! -e Local/ShakeConfig.hs || -L Local/ShakeConfig.hs ]]; then
        ln -sf ${shakeConfig} Local/ShakeConfig.hs
      else
        echo "not replacing plain file: Local/ShakeConfig.hs"
      fi
    '';
  };
}
