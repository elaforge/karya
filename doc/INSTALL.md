## Installation

- Install GHC.  At least 8.0 and 8.4 should work.  8.2 definitely does not
work.  It should compile fine, but the REPL won't work.  Details at
<https://ghc.haskell.org/trac/ghc/ticket/13604>.

- Install [non-haskell dependencies](#non-haskell-dependencies).

- Update `Local/ShakeConfig.hs` to point to where those dependencies are
installed.

- Install [haskell dependencies](#haskell-dependencies).

- Run `tools/setup-empty`.  Read it if you want, it's short.

- Build shakefile: `bin/mkmk`

- Build optimized binaries: `bin/mk binaries`.  It will try to link to CoreMIDI
on the mac and to JACK on linux.  If for some reason you don't have either of
those, you can run `midi=stub bin/mk` to link the stub MIDI driver, but now it
will never produce any MIDI so what was the point?

- On OS X, run `defaults write -g ApplePressAndHoldEnabled -bool false` to
re-enable key repeats globally.  Provided you want them to work sanely, and
not iphone-ly.

- Go read `doc/quickstart.md`.

- Read `doc/DEVELOPMENT.md` if you want to do some of that.

## Non-Haskell dependencies

- Git, and make sure `user.email` and `user.name` are configured.

- Install either via package manager or manually:

    - fltk - I use >=1.3.4, but any >=1.3 should work.
    - libpcre

    Remember to install -dev variants to get headers.  If your package manager
    puts headers in a non-standard place, e.g. `~/homebrew`, then
    `cabal install --only-dependencies` won't find it.  You'll need to add
    flags, e.g.:

        cabal install --extra-include-dirs=$HOME/homebrew/include \
            --extra-lib-dirs=$HOME/homebrew/lib --only-dependencies

- lilypond for the lilypond backend.  This is optional.

- The bravura font for music symbols: <http://www.smufl.org/fonts/> and
Noto for any other kind of symbol: <https://www.google.com/get/noto/>.
I don't use fancy symbols very much, so they're not essential.

    OS X: `cp *.otf ~/Library/Fonts` or use FontBook to install them.

    Linux: `cp *.otf ~/.fonts # or /usr/share/fonts`

    On linux, use `fc-list` to see installed fonts and their names.  For some
reason, the fonts on linux sometimes have backslashes in their names, and
sometimes not.  If there is a complaint at startup about the font not being
found you might have to edit `App/Config.hsc`.

## Linux

- Either jack1 or jack2.  JACK support is mostly untested and probably doesn't
work, since I don't do music on linux.  Get in touch if you can help with linux
support.

## Haskell dependencies

You can do this either the cabal way, or the stack way.

### cabal way

For whatever reason cabal doesn't install binary dependencies automatically,
and it doesn't know how to do dependencies between them, so first run:

    # These must be separate command lines!  Cabal is not smart about binaries.
    cabal install alex happy
    cabal install c2hs cpphs

I think when I can rely on Cabal >=2.0 and nix-style builds, `cabal install`
will finally handle this automatically.

To install the needed haskell dependencies, type:

    cabal sandbox init  # if you're afraid to screw up your haskell installation
    cp data/cabal.config . # if you want to use the same versions as I do
    cabal install --only-dependencies

The actual build is with shake, but there's a dummy cabal file with just
dependencies to make install easier.

If you want to build the documentation:

    cabal install hscolour
    my-package-manager install pandoc

You can also install pandoc with cabal but it has a ridiculous number of
dependencies.

### stack way

I don't use stack, but I added some basic support so hopefully this should work:

    stack setup # install ghc
    stack install alex happy c2hs cpphs
    # Put ~/.local/bin in $PATH if stack warns you about that.
    stack install --only-dependencies

From here on, you will need to `export use_stack=t`.  Then when you run
`bin/mkmk` and `bin/mk`, it will use `stack path` to find where to get the
stack-flavored ghc and packages.

### nix way

If you like nix, write me a nix expression.  You could probably get this whole
mess down to one command.

## 音, Im, Synth

These are all names for the incomplete offline synthesizer.  It requires a
bunch of extra dependencies.  First you need more non-haskell dependencies.
Get the -dev versions as usual:

    - faust
    - libsamplerate
    - libsndfile

Turn on `enableIm` in `Local.ShakeConfig`, and add a bunch more haskell deps:

    cp data/all-deps.cabal karya.cabal
    cp data/cabal.config.all-deps cabal.config # if you want my versions
    cabal install --only-dependencies

Or, if you're doing it the stack way:

    cp data/all-deps.cabal karya.cabal
    vi stack.yaml # uncomment the stuff in there that says to

## Misc

`tools/run_profile` expects `ps2pdf` in the path, which is part of ghostscript.
It's fine if it's not, but you'll get ps instead of pdf.  Help with heap
profiling would be very welcome!
