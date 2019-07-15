# Incomplete nix to gather all the dependencies.

# Examples:
# nix-shell --attr build-env --arg withIm true
# nix-store -r $(nix-instantiate --attr libsamplerate-elaforge)
# nix build -f default.nix libsamplerate

{ nixpkgs ? import <nixpkgs> {}
, withLilypond ? false # This drags in all of texlive!
, withIm ? false # TODO sync this with Shake.Config.enableIm
, withEkg ? false # ekg is really heavy
, withDocs ? false
}:

let
    ghc = nixpkgs.haskell.packages.ghc844;

    # util
    guard = bool: list: if bool then list else [];
    splitOn = sep: str:
        builtins.filter builtins.isString (builtins.split sep str);
    lines = str: builtins.filter (s: s != "") (splitOn "\n" str);
    readLines = fn: lines (builtins.readFile fn);
in rec {
    basicDeps = with nixpkgs; [
        hackage2
        git
        fltk
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

    # libsamplerate with my patches to save and resume. The official one is
    # nixpkgs.libsamplerate.
    # I compile without libsndfile and none of the utils, so no deps on e.g.
    # CoreServices are needed.
    libsamplerate-elaforge = nixpkgs.stdenv.mkDerivation {
        name = "libsamplerate-elaforge";
        src = builtins.fetchGit {
            url = "git@github.com:elaforge/libsamplerate.git";
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
    disableTest = drv:
        if drv == null then drv
        else nixpkgs.haskell.lib.overrideCabal drv (drv: {
            doCheck = false;
            doBenchmark = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
            # doHaddock = false;
            broken = false;
        });

    notBroken = pkg: nixpkgs.haskell.lib.overrideCabal pkg
        (_: { broken = false; });
    hackage = ghc.ghcWithPackages (pkgs: with pkgs; map disableTest [
        Diff
        QuickCheck
        aeson
        ansi-terminal
        array
        async
        attoparsec
        base
        base64-bytestring
        binary
        bytestring
        c-storable
        cereal
        colour
        concurrent-output
        containers
        data-ordlist
        deepseq
        digest
        directory
        dlist
        extra
        fclabels
        filepath
        # ghc
        ghc-events
        ghc-paths
        ghc-prim
        hashable
        haskeline
        haskell-src
        hedgehog
        hlibgit2
        # med-module is marked broken, but it's not.
        (notBroken med-module)
        megaparsec
        mersenne-random-pure64
        mtl
        network
        old-locale
        parser-combinators
        pcre-heavy
        pcre-light
        pretty
        process
        random
        random-shuffle
        semigroups
        shake
        stm
        terminfo
        text
        time
        transformers
        unix
        utf8-string
        vector
        wcwidth
        # writer-cps-mtl 0.1.1.6 gets
        # writer-cps-transformers, which wants transformers >=0.5.6.0
        # but ghc 8.4.4 has transformers-0.5.5.0
        # So I have to use writer-cps-transformers <=0.5.5.0
        # writer-cps-mtl
        zlib
        zmidi-core
    ]);

    wantPkg = pkg:
        # nix gets the "ghc" package confused with the compiler.
        pkg != "ghc"
        # writer-cps-mtl 0.1.1.6 gets
        # writer-cps-transformers, which wants transformers >=0.5.6.0
        # but ghc 8.4.4 has transformers-0.5.5.0
        # So I have to use writer-cps-transformers <=0.5.5.0
        && pkg != "writer-cps-mtl";

    hackage2 = ghc.ghcWithPackages (pkgs: map (pkg: disableTest pkgs."${pkg}") (
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
        buildInputs = deps;
    };
}
