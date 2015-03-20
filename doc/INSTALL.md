## INSTALLATION

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

To install the needed haskell dependencies, type:

    cabal install --only-dependencies

The actual build is with shake, but there's a dummy cabal file with just
dependencies to make install easier.

If you want to build the documentation:

    cabal install hscolour pandoc

These are separate because pandoc has a ridiculous number of dependencies and
all it does is turn the `doc/*.md` files into html.

I link to the `ghc` package, which means that the versions of the GHC boot
packages are all fixed to the versions included with GHC.  So you can't upgrade
bytestring or containers or whatever.  Otherwise other packages will want to
link to the new versions, but the `ghc` dependency forces karya to link to the
old version, and you can't link to two versions at the same time.  If you do
other haskell development you may want to use a cabal sandbox.  It might need
special support to work with the GHC API though.  I've never used it so I don't
know.

## INSTALLED SEPARATELY

fltk-1.3, from <http://fltk.org/>.  Install the latest version from SVN for
bugfixes and better rendering on retina macs.  The shakefile expects to use
the uninstalled library via `/usr/local/src/fltk-1.3/fltk-config`.

libpcre library from <http://www.pcre.org>.

If your package manager puts the headers in a non-standard place, then
the cabal build for the haskell bindings won't find it.  So you need flags:

    % cabal configure --extra-include-dirs=$HOME/homebrew/include \
        --extra-lib-dirs=$HOME/homebrew/lib

libgit2 - It has no official release, but as of 2014-03-12 my binding is to
v0.20.0.  You can get it with:

    % git clone https://github.com/libgit2/libgit2
    % cd libgit2
    % git checkout tags/v0.20.0
    % git checkout -b v0.20.0

Then build and install according to their instructions.

lilypond, for some fonts, and for the lilypond backend, of course.
Lilypond sometimes changes the position of characters in their fonts, which
causes symbols to break.  As of 2014-07-02 I'm using the fonts from 2.18.2.

OS X:

    cp LilyPond.app/Contents/Resources/share/lilypond/current/fonts/otf/emmentaler* ~/Library/Fonts

Linux:

    cp /usr/share/lilypond/<version>/fonts/otf/ ~/.fonts
    # or
    cp /usr/share/lilypond/<version>/fonts/otf/ /usr/share/fonts/

Use fc-list to see installed fonts and their names.  For some reason, the fonts
on linux sometimes have backslashes in their names, and sometimes not.  If
there is a complaint at startup about the font not being found you might have
to edit App/Config.hsc.  Or not, I hardly use the music symbols.

## LINUX

Either jack1 or jack2.  JACK support is mostly untested and probably doesn't
work, since I don't do music on linux.  Get in touch if you care about linux
support.

## MISC

`tools/run_profile` expects `ps2pdf` in the path, which is part of ghostscript.
It's fine if it's not, but you'll get ps instead of pdf.
