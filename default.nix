# Incomplete nix to gather all the dependencies.

# Examples:
# nix-shell --attr build-env --arg withIm true
# nix-store -r $(nix-instantiate --attr libsamplerate-elaforge)
# nix build -f default.nix --arg withIm true --arg withDocs true build-env

{ nixpkgs ? import <nixpkgs> {}
, withLilypond ? false # This drags in all of texlive!
, withIm ? false # TODO sync this with Shake.Config.enableIm
, withEkg ? false # ekg is really heavy
, withDocs ? false
}:

let
    ghc = let p = nixpkgs.haskell.packages;
        in if p?ghc844 then p.ghc844 else p.ghc842;

    # config = {
    #     packageOverrides = pkgs: rec {
    #         haskellPackages = pkgs.haskellPackages.override {
    #             overrides = newPkgs: oldPkgs: rec {
    #             };
    #         };
    #     };
    # };

    # util
    guard = bool: list: if bool then list else [];
    splitOn = sep: str:
        builtins.filter builtins.isString (builtins.split sep str);
    lines = str: builtins.filter (s: s != "") (splitOn "\n" str);
    readLines = fn: lines (builtins.readFile fn);
in rec {
    basicDeps = with nixpkgs; [
        fltk
        git
        hackage
        pcre
    ];

    fontDeps = with nixpkgs; [
        noto-fonts
        openlilylib-fonts.bravura
    ];

    docDeps = [
        ghc.hscolour
        nixpkgs.pandoc
    ];

    libsamplerate-elaforge = nixpkgs.stdenv.mkDerivation {
        # libsamplerate with my patches to save and resume. The official one is
        # nixpkgs.libsamplerate.
        # I compile without libsndfile and none of the utils, so no deps on
        # e.g.  CoreServices are needed.
        name = "libsamplerate-elaforge";
        src = builtins.fetchGit {
            url = "https://github.com/elaforge/libsamplerate.git";
            rev = "fb2e4db83202943219e520f3e85013dccf907e15";
            ref = "local";
        };

        # Without pkgconfig, I get "error: macro PKG_INSTALLDIR is not
        # defined".
        nativeBuildInputs = with nixpkgs; [autoreconfHook pkgconfig];
        configureFlags = ["--enable-shared=no" "--enable-static=yes"];
    };
    # Output will be include/samplerate.h lib/libsamplerate.a

    faust-elaforge = import nix/faust.nix { inherit nixpkgs; };

    imDeps = with nixpkgs; [
        faust-elaforge
        libsamplerate-elaforge
        libsndfile
        liblo
        # This is a build dep, not a library dep.
        ghc.c2hs
    ];

    # Turn off all tests.
    # The test for Diff is broken and since I'm going to have to compile
    # everything anyway, I don't need tests.
    # TODO: I should probably override all of the haskellPackages with this,
    # or else dependencies will still compile with tests and profiling.
    # How do I do that for the instance of haskellPackages that comes out of
    # ghc###?  Or are they all from the same place?
    disableTest = drv:
        # ghc bootlibs show up in pkgs as nulls.
        if drv == null then drv
        else nixpkgs.haskell.lib.overrideCabal drv (drv: {
            doCheck = false;
            doBenchmark = false;
            enableExecutableProfiling = false;
            # I might want library profiling.
            enableLibraryProfiling = false;
            # I do want haddock, to link docs.
            # doHaddock = false;

            # med-module is marked broken, but it's not.
            broken = false;
        });

    wantPkg = pkg:
        # nix gets the "ghc" package confused with the compiler.
        pkg != "ghc"
        # writer-cps-mtl 0.1.1.6 gets
        # writer-cps-transformers, which wants transformers >=0.5.6.0
        # but ghc 8.4.4 has transformers-0.5.5.0
        # So I have to use writer-cps-transformers <=0.5.5.0
        && pkg != "writer-cps-mtl";

    hackage = ghc.ghcWithPackages (pkgs: map (pkg: disableTest pkgs."${pkg}") (
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
    build-env = nixpkgs.stdenv.mkDerivation {
        name = "build-env";
        builder = "${nixpkgs.bash}/bin/bash";
        args = [(builtins.toFile "build-env-builder.sh" ''
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
        buildInputs = [nixpkgs.coreutils];
    };
}
