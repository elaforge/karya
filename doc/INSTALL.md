## Installation

- Install GHC.  At least 8.0 and 8.4.4 should work.  8.2 definitely does not
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
not iphone-ly.  Or don't do that.  This is just reminder to myself.

- Go read `doc/quickstart.md`.

- Read `doc/DEVELOPMENT.md` if you want to do some of that.

## Non-Haskell dependencies

- Git, and make sure `user.email` and `user.name` are configured.

- Install either via package manager or manually:

    - fltk - I use >=1.3.4, but any >=1.3 should work.
    - libpcre

    Install -dev variants to get headers.  If your package manager puts headers
    in a non-standard place, e.g. `~/homebrew`, then `cabal install
    --only-dependencies` won't find it.  You'll need to add flags, e.g.:

        cabal install --extra-include-dirs=$HOME/homebrew/include \
            --extra-lib-dirs=$HOME/homebrew/lib --only-dependencies

- lilypond for the lilypond backend.  This is optional.

- The bravura font for music symbols:
<https://github.com/steinbergmedia/bravura/tree/master/releases> (the main page
is <http://www.smufl.org/fonts/>), and Noto for any other kind of symbol:
<https://www.google.com/get/noto/>.  I don't use fancy symbols very much, so
they're not essential.  You'll probably get some complaints at startup if
they're missing, you can ignore that.

    OS X: `cp *.otf ~/Library/Fonts` or use FontBook to install them.

    Linux: `cp *.otf ~/.fonts # or /usr/share/fonts`

    On linux, use `fc-list` to see installed fonts and their names.  For some
reason, the fonts on linux sometimes have backslashes in their names, and
sometimes not.  If there is a complaint at startup about the font not being
found you might have to edit a font name in `App/Config.hsc`.

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

    stack setup # stack.yaml uses 8.4.4
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

- faust - Faust had major stdlib changes a few years back, and if you use a
conservative distro, the bundled one may be too old.  Install by hand to be
sure.  I'm using `2.5.34`.

- libsamplerate - I use a local fork, with support for saving and restoring
state.  Get the `local` branch from
https://github.com/elaforge/libsamplerate/tree/local and clone to
`/usr/local/src/libsamplerate`, unless you want mess with config.  Build with
the usual `./autogen.sh && ./configure && make`.  Don't install, the shakefile
will link directly to it.  The reason is that it's common to have standard
libsamplerate installed, and I don't want to mess up your system.

    If you cloned someplace other than `/usr/local/src/libsamplerate`, you'll
need to update Local/ShakeConfig.hs update the `libsamplerate` field with
the link and compile flags.

- libsndfile - Standard install, use your package manager or whatever.

Turn on `enableIm` in `Local.ShakeConfig`, and add a bunch more haskell deps:

    cd doc/cabal && cabal install --only-dependencies

Or, if you're doing it the stack way:

    cp doc/cabal/all-deps.cabal karya.cabal
    vi stack.yaml # uncomment the stuff in there that says to

- liblo - for OSC

## Misc

`tools/run_profile` expects `ps2pdf` in the path, which is part of ghostscript.
It's fine if it's not, but you'll get ps instead of pdf.  Help with heap
profiling would be very welcome!
