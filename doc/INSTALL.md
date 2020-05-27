## the nix way

Install nix and cachix.  I upload build results to `cachix` so if you use
that you can avoid building them.  Of course if you like building you can
skip all the `cachix` steps:

    # Standard nix install, skip if you already have nix:
    bash <(curl -L https://nixos.org/nix/install)
    # On my laptop, nix installed with a max-jobs of 32, which is nuts on a
    # laptop with only 4 cores, and totally wedges it up.
    # Edit /etc/nix/nix.conf and possibly get max-jobs under control.

    # Install cachix, skip if you like building
    nix-env -iA cachix -f https://cachix.org/api/v1/install
    # Configure nix to use my cachix cache.
    # sudo is necessary because it wants to modify /etc/nix/nix.conf.
    sudo cachix use elaforge
    # get nix-daemon to see the new config:
    systemd-linux> sudo systemctl restart nix-daemon
    osx> sudo launchctl stop org.nixos.nix-daemon
    osx> sudo launchctl start org.nixos.nix-daemon

    # Actually do the install/build:
    tools/nix-enter

This will download tons of stuff, and drop you in a subshell where that stuff
is available.  After this you'll need to run `tools/nix-enter` whenever you
want to build.  I use a special color on `PS1` when `SHLVL` > 1 to indicate
the subshell.

This gets the "everything" build including "im" below.  Since my build file
is a mess, the non-im build is broken and I don't feel like fixing it at the
moment.  I'll probably just make the everything build the only build, now that
nix makes it easy.

Now do the rest of the build steps, same as "the traditional way" below:

- Run `tools/setup-empty`.  Read it if you want, it's short.

- `tools/nix-enter` has already created a `Local/ShakeConfig.hs`.  Read it if
you want.

- Build shakefile: `bin/mkmk`

- Build optimized binaries: `bin/mk binaries`.  It will try to link to CoreMIDI
on the mac and to JACK on linux.  If for some reason you don't have either of
those, you can run `midi=stub bin/mk` to link the stub MIDI driver, but now it
will never produce any MIDI so what was the point?  Even with the "im" backend,
it uses MIDI to tell the DAW when to start playing.  You could go totally
non-MIDI by bypassing the DAW and just play the output audio directly,
`build/opt/stream_audio` will do that.

- Go read `doc/quickstart.md`.

- Read `doc/DEVELOPMENT.md` if you want to do some of that.

## the traditional way

This is a lot more work than the nix way!

- Install GHC.  I'm using 8.8 now and I dropped support for previous versions.
8.2 definitely does not work, details at
<https://ghc.haskell.org/trac/ghc/ticket/13604>.

- Install [non-haskell dependencies](#non-haskell-dependencies).

- Run `tools/setup-empty`.  Read it if you want, it's short.

- Update `Local/ShakeConfig.hs` to point to where those dependencies are
installed.

- Install [haskell dependencies](#haskell-dependencies).

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

        cabal v1-install --extra-include-dirs=$HOME/homebrew/include \
            --extra-lib-dirs=$HOME/homebrew/lib --only-dependencies

- lilypond for the lilypond backend.  This is optional.  If you never try to
compile a score via lilypond it will never notice you're missing this
dependency.

- The bravura font for music symbols:
<https://github.com/steinbergmedia/bravura/tree/master/releases> (the main page
is <http://www.smufl.org/fonts/>), and Noto for any other kind of symbol:
<https://www.google.com/get/noto/>.  I don't use fancy symbols very much, so
they're not essential.  You'll probably get some complaints at startup if
they're missing, it's harmless to ignore them.  You might see some boxes
instead of symbols if you use one of the few calls or scales that use non-ASCII
symbols.

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

New cabal with nix-style builds might have fixed this.

To install the needed haskell dependencies, type:

    cabal sandbox init  # if you're afraid to screw up your haskell installation
    cabal v1-install --only-dependencies

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
libsamplerate installed in /usr/lib or /usr/local/lib, and I don't want to mess
up your system.

    If you cloned someplace other than `/usr/local/src/libsamplerate`, you'll
need to update Local/ShakeConfig.hs update the `libsamplerate` field with
the link and compile flags.

- libsndfile, liblo - Standard installs, use your package manager.

Turn on `enableIm` in `Local.ShakeConfig`, and add a bunch more haskell deps:

    cd doc/cabal && cabal install --only-dependencies

Or, if you're doing it the stack way:

    cp doc/cabal/all-deps.cabal karya.cabal
    vi stack.yaml # uncomment the stuff in there that says to

## Misc

`tools/run_profile` expects `ps2pdf` in the path, which is part of ghostscript.
It's fine if it's not, but you'll get ps instead of pdf.  Help with heap
profiling would be very welcome!
