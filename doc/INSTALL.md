## INSTALLATION

- If you pulled from darcs and didn't use --set-scripts-executable, run
`chmod 755 fix-perms && ./fix-perms`.

- Install dependencies, documented below.

- Build shakefile: `bin/mkmk`

- Build optimized binaries and documentation: `bin/mk binaries doc`.  It will
try to link to CoreMIDI on the mac and to JACK on linux.  If for some reason
you don't have either of those, you can pass `midi=stub` to link the stub MIDI
driver, but don't expect any interesting music to come out.

- On OS X, run `defaults write -g ApplePressAndHoldEnabled -bool false` to
re-enable key repeats globally.  Provided you want them to work sanely, and
not iphone-ly.

## HASKELL DEPENDENCIES

For whatever reason cabal won't install happy automaticaly, so first run

    cabal install happy

To install the needed haskell dependencies, type:

    cabal sandbox init  # if you're afraid to screw up your haskell installation
    cabal install --only-dependencies

The actual build is with shake, but there's a dummy cabal file with just
dependencies to make install easier.

You might get an error about `pcre.h` file not found, in that case see libpcre
below.

If you want to build the documentation:

    cabal install hscolour pandoc

These are separate because pandoc has a ridiculous number of dependencies and
all it does is turn the `doc/*.md` files into html.  pandoc has so many
dependencies it might break your install by downgrading things, so maybe do
that in a separate sandbox.

## INSTALLED SEPARATELY

- fltk-1.3, from <http://fltk.org/>.  Install the latest version from SVN
for bugfixes and better rendering on retina macs.  The shakefile expects to
use the uninstalled library via `/usr/local/src/fltk-1.3/fltk-config`.

- libpcre library from <http://www.pcre.org>.

    If your package manager puts the headers in a non-standard place, e.g.
~/homebrew, then the cabal build for the haskell bindings won't find it.  So
you need flags:

        % cabal install --extra-include-dirs=$HOME/homebrew/include \
            --extra-lib-dirs=$HOME/homebrew/lib --only-dependencies

- lilypond, for some fonts, and for the lilypond backend, of course.
Lilypond sometimes changes the position of characters in their fonts, which
causes symbols to break.  As of 2014-07-02 I'm using the fonts from 2.18.2.

    OS X: `cp LilyPond.app/Contents/Resources/share/lilypond/current/fonts/otf/emmentaler* ~/Library/Fonts`

    Linux: `cp /usr/share/lilypond/<version>/fonts/otf/ ~/.fonts # or /usr/share/fonts`

    Use `fc-list` to see installed fonts and their names.  For some reason, the
fonts on linux sometimes have backslashes in their names, and sometimes not.
If there is a complaint at startup about the font not being found you might
have to edit App/Config.hsc.  Or not, I hardly use the music symbols.

## LINUX

Either jack1 or jack2.  JACK support is mostly untested and probably doesn't
work, since I don't do music on linux.  Get in touch if you care about linux
support.

## MISC

`tools/run_profile` expects `ps2pdf` in the path, which is part of ghostscript.
It's fine if it's not, but you'll get ps instead of pdf.
