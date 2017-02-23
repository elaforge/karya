## INSTALLATION

- If you pulled from darcs and didn't use --set-scripts-executable, run
`chmod 755 fix-perms && ./fix-perms`.

- Install dependencies, documented below in `INSTALLED SEPARATELY`.

- Update `Shake/Config.hs` to point to where those dependencies are installed.

- Build shakefile: `bin/mkmk`

- Build optimized binaries and documentation: `bin/mk binaries doc`.  It will
try to link to CoreMIDI on the mac and to JACK on linux.  If for some reason
you don't have either of those, you can pass `midi=stub` to link the stub MIDI
driver, but now it will never produce any MIDI so what was the point?

- On OS X, run `defaults write -g ApplePressAndHoldEnabled -bool false` to
re-enable key repeats globally.  Provided you want them to work sanely, and
not iphone-ly.

## HASKELL DEPENDENCIES

For whatever reason cabal won't install happy automatically, so first run

    cabal install happy

The reason for happy is some dumb thing like needing `haskell-src` to do pretty
printing, or something like that.  To install the needed haskell dependencies,
type:

    cabal sandbox init  # if you're afraid to screw up your haskell installation
    cabal install --only-dependencies

The actual build is with shake, but there's a dummy cabal file with just
dependencies to make install easier.

You might get an error about `pcre.h` file not found, in that case see libpcre
below.

If you want to build the documentation:

    cabal install hscolour pandoc

These are separate because pandoc has a ridiculous number of dependencies.
pandoc has so many dependencies it might break your install by downgrading
things, so maybe do that in a separate sandbox, or at least --dry-run first.
It's a completely independent binary, so however you can get it compiled and
installed is fine.

## INSTALLED SEPARATELY

- Fltk, from <http://fltk.org/>.  The latest 1.3 should work fine.  I use the
SVN version because in the past its had bugfixes for retina macs.
`Shake/Config.hs` needs the path to the `fltk-config` script, which is normally
going to be `/usr/local/bin` if you did a `make install`.

- libpcre library from <http://www.pcre.org>.

    If your package manager puts the headers in a non-standard place, e.g.
~/homebrew, then the cabal build for the haskell bindings won't find it.  So
you need flags:

        % cabal install --extra-include-dirs=$HOME/homebrew/include \
            --extra-lib-dirs=$HOME/homebrew/lib --only-dependencies

- lilypond for the lilypond backend.  This is optional.

- The bravura font for music symbols: <http://www.smufl.org/fonts/>

    OS X: `cp *.otf ~/Library/Fonts` or use FontBook to install them.

    Linux: `cp *.otf ~/.fonts # or /usr/share/fonts`

    On linux, use `fc-list` to see installed fonts and their names.  For some
reason, the fonts on linux sometimes have backslashes in their names, and
sometimes not.  If there is a complaint at startup about the font not being
found you might have to edit App/Config.hsc.  Or not, I hardly use the music
symbols.

## LINUX

Either jack1 or jack2.  JACK support is mostly untested and probably doesn't
work, since I don't do music on linux.  Get in touch if you care about linux
support.

## 音, Im, Synth

These are all names for the incomplete offline synthesizer.  It requires a bunch
of extra dependencies.  To build, enable it in Shake.Config, install the VST3
SDK from Steinberg's website, and install additional deps from
build/enabled-deps.karya.

But it doesn't do much yet.

## MISC

`tools/run_profile` expects `ps2pdf` in the path, which is part of ghostscript.
It's fine if it's not, but you'll get ps instead of pdf.  Help with heap
profiling would be very welcome!
