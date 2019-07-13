# Incomplete nix to gather all the dependencies.

# Examples:
# nix-shell --attr build-env --arg withIm true
# nix-store -r $(nix-instantiate --attr libsamplerate)
# nix build -f default.nix libsamplerate

# TODO pass allowUnsupportedSystem, for faust2 I think
{ nixpkgs ? import <nixpkgs> {}
, withLilypond ? false # This drags in all of texlive!
, withIm ? false # TODO sync this with Shake.Config.enableIm
, withDocs ? false
}:

let
    ghc = nixpkgs.haskell.packages.ghc844;

    # util
    guard = bool: list: if bool then list else [];
in rec {
    basicDeps = with nixpkgs; [
        hackage
        git
        fltk
    ];

    fontDeps = [
        # noto
        # bravura
    ];

    docDeps = with nixpkgs; [
        hscolour
        pandoc
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

        # Without pkgconfig, I get "error: macro PKG_INSTALLDIR is not defined".
        nativeBuildInputs = with nixpkgs; [autoreconfHook pkgconfig];
        configureFlags = ["--enable-shared=no" "--enable-static=yes"];
    };
    # Output will be include/samplerate.h lib/libsamplerate.a

    imDeps = with nixpkgs; [
        # Crashes with:
        # bin/bash: libtool: command not found
        # Probably just need to patch that into inputs.
        # faust2

        libsamplerate-elaforge
        libsndfile
        liblo

        # this is from ghcPackages, but it's a binary build dep, not a library.
        # c2hs
    ];

    hackage = ghc.ghcWithPackages (pkgs: with pkgs; [
        # won't compile:
        #   "Couldn't match expected type ‘Double’ with actual type ‘Bool’"
        # Maybe due to unpinned versions, but maybe Diff just has broken tests?
        # Diff

        QuickCheck
        # aeson
        # ansi-terminal
        # array
        # async
        # attoparsec
        # # base
        # base64-bytestring
        # binary
        # bytestring
        # c-storable
        # cereal
        # colour
        # concurrent-output
        # containers
        # data-ordlist
        # deepseq
        # digest
        # directory
        # dlist
        # extra
        # fclabels
        # filepath
        # # ghc
        # ghc-events
        # ghc-paths
        # ghc-prim
        # hashable
        # haskeline
        # haskell-src
        # hedgehog
        # hlibgit2
        # med-module
        # megaparsec
        # mersenne-random-pure64
        # mtl
        # network
        # old-locale
        # parser-combinators
        # pcre-heavy
        # pcre-light
        # pretty
        # process
        # random
        # random-shuffle
        # semigroups
        # shake
        # stm
        # terminfo
        # text
        # time
        # transformers
        # unix
        # utf8-string
        # vector
        # wcwidth
        # writer-cps-mtl
        zlib
        zmidi-core
    ]);

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
