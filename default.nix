# Incomplete nix to gather all the dependencies.
# See TODO nix: for current status.
# https://www.srid.ca/haskell-nix.html

# Examples:
# nix-shell -command zsh --attr buildEnv --arg withIm true --arg withDocs true
# nix-store -r $(nix-instantiate --attr libsamplerate)
# nix build -f default.nix --arg withIm true --arg withDocs true buildEnv

# Building lilypond drags in all of texlive.  It's also marked broken on
# darwin for 19.09.
{ withLilypond ? false
, withIm ? false # TODO sync this with Shake.Config.enableIm
, withEkg ? false # ekg is really heavy
, withDocs ? false
, profiling ? false # enable profiling in dependent libraries
}:

let
  ghcVersion = "ghc864";
  config = let
    overrideCabal = nixpkgs.haskell.lib.overrideCabal;
    override = old: {
      med-module = overrideCabal old.med-module (drv: {
        # patches = [nix/med-module.patch];
        broken = false;
      });
    };
  in {
    # For nixpkgs.mkl, for mesh2faust.
    allowUnfree = true;
    # allowBroken = true;
    packageOverrides = pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${ghcVersion}" = pkgs.haskell.packages."${ghcVersion}".override {
            overrides = new: old: override old;
          };
        };
      };
    };
  };

  nixpkgs = import nix/nixpkgs.nix { inherit config; };

  ghc = nixpkgs.haskell.packages."${ghcVersion}";

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
    # writer-cps-mtl 0.1.1.6 gets
    # writer-cps-transformers, which wants transformers >=0.5.6.0
    # but ghc 8.4.4 has transformers-0.5.5.0
    # So I have to use writer-cps-transformers <=0.5.5.0
    # && pkg != "writer-cps-mtl";
    ;

  hackage = ghc.ghcWithPackages (pkgs: map (pkg: pkgs."${pkg}") (
    builtins.filter wantPkg (builtins.concatLists [
      (readLines doc/cabal/basic)
      (guard withIm (readLines doc/cabal/im))
      (guard withEkg ["ekg"])
    ])
  ));

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
    args = [(builtins.toFile "buildEnv-builder.sh" ''
      # TODO surely I can use a standard builder script instead.
      PATH=""
      for p in $buildInputs; do
          export PATH=$p/bin''${PATH:+:}$PATH
      done

      set -eux
      mkdir $out
      cd $out
      for src in $srcs; do
          ln -s $src $(basename $src)
      done
    '')];
    srcs = deps;
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
