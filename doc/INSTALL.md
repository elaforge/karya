## the nix way

On linux, install either jack1 or jack2.  JACK support is mostly untested and
probably doesn't work, since I don't do music on linux.  Get in touch if you
can help with linux support.

Install nix and cachix.  I upload build results to `cachix` so if you use
that you can avoid building them.  Of course if you like building you can
skip all the `cachix` steps:

    ```sh
    ### Standard nix install, skip if you already have nix.  Or go to the nix
    # site and find whatever is current for this:
    bash <(curl -L https://nixos.org/nix/install)
    # On my laptop, nix installed with a max-jobs of 32, which is nuts on a
    # laptop with only 4 cores, and totally wedges it up.
    # Edit /etc/nix/nix.conf and possibly get max-jobs under control.

    ### Install cachix, skip if you like building:
    nix-env -iA cachix -f https://cachix.org/api/v1/install
    # Configure nix to use my cachix cache.
    # sudo is necessary because it wants to modify /etc/nix/nix.conf.
    sudo cachix use elaforge
    # Get nix-daemon to see the new config.  This may be unnecessary if
    # you did a single user nix install above:
    systemd-linux> sudo systemctl restart nix-daemon
    osx> sudo launchctl stop org.nixos.nix-daemon
    osx> sudo launchctl start org.nixos.nix-daemon

    ### Actually do the install:
    tools/nix-enter
    ```

This will download tons of stuff, and drop you in a subshell where that stuff
is available.  After this you'll need to run `tools/nix-enter` whenever you
want to build.  I use a special color on `PS1` when `SHLVL` > 1 to indicate
the subshell.

On OS X, by default you do not even have to install the commandline compiler
tools, because it will use the ones from nix.

This gets the "everything" build including "im" below.  Since my build file is
a mess, the non-im build is broken and I don't feel like fixing it at the
moment.  I'll probably just make the everything build the only build, now that
nix makes it easy.

Now do the rest of the build steps, same as "the traditional way" below:

- Install the "bravura" font:

    ```sh
    nix-build default.nix -A fontDeps
    osx> cp $(find -L result* -name '*.otf') ~/Library/Fonts # or use FontBook
    linux> cp $(find -L result* -name '*.otf') ~/.fonts
    # I don't actually know how to install fonts on linux.  The above doesn't
    # work on nixos, instead add openlilylib-fonts.bravura to configuration.nix.
    ```

- Run `tools/setup-generic`.  Read it if you want, it's short.

- `tools/nix-enter` has already created a `Local/ShakeConfig.hs`.  Read it if
you want.

- Build shakefile: `bin/mkmk`

- Build optimized binaries: `bin/mk binaries`.  It will try to link to CoreMIDI
on the mac and to JACK on linux.  If for some reason you don't have either of
those or they don't work, you can run `midi=stub bin/mk` to link the stub MIDI
driver.  This could be useful if you are using `im` only, and don't want to
deal with JACK.  In that case, you can go non-MIDI and turn on
`LCmd.im_play_direct` to have karya play the audio itself.

- Go read `doc/quickstart.md`.

- Read `doc/DEVELOPMENT.md` if you want to do some of that.

Ignore the rest of this file!

## the traditional way

This is more work than the nix way!  But you won't have to install nix, or run
`tools/nix-enter` all the time, and you'll have all the haskell stuff globally
available.

- On OS X, install commandline tools if you haven't already:
    `xcode-select --install`

- Install GHC, either the traditional way or `ghcup`.  I'm using 9.12 now.

- Install [non-haskell dependencies](#non-haskell-dependencies).

- Run `tools/setup-generic`.  Read it if you want, it's short.

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

Here's my latest experience installing this way on M3 aarch64 OSX, using brew:

  * brew install fltk
  * brew install pkgconf
  * brew install portaudio # for bindings-portaudio
  * brew install pcre # for pcre-light
  * brew install openssl # for hlibgit2
  * brew install libsndfile # for hsndfile
  * brew now installs into /opt by default. Edit ~/.cabal/config to find it:
    ```
    extra-include-dirs: /opt/homebrew/include
    extra-lib-dirs: /opt/homebrew/lib
    extra-prog-path: /opt/homebrew/bin
    ```
  * After running `tools/setup-generic`, which will create
    Local/ShakeConfig.hs, and put the same paths in it:
    ```
    , globalIncludes = ["/opt/homebrew/include"]
    , globalLibDirs = ["/opt/homebrew/lib"]
    ```
  * # if you are not on ghc 9.6, let cabal pick new versions
    rm cabal.project.freeze
  * cabal build --only-dep
  * cabal freeze # update cabal.project.freeze if you deleted it

  - Install deps for im:
  * brew install rubberband # librubberband, for sampler
  * brew install autoconf automake libtool
  * git clone https://github.com/elaforge/libsamplerate to /usr/local/src
    cd libsamplerate && ./configure && make
  * cabal install cpphs c2hs
  * brew install faust # for im faust backend
  * hsc2hs-9.2.8 on aarch64 OSX is broken!  Edit `hsc2hs` directly and put
    a space in `HSC2HS_EXTRA` between `--cflag` and `--lflag`.
    Future versions don't seem to have the problem.

Upgrading ghc version:
  * ghcup install, ghcup set
  * rm -rf build
  * rm cabal.project.freeze # apparently can't have >1 of these?
  * cabal update
  * cabal build --only-dep
  * cabal freeze # update cabal.project.freeze if you deleted it

I previously had trouble with `hlibgit2` on the `cabal build` line.
The last time I tried it though, it worked without any changes to
`cabal.project`.  But if it comes back, for reference, the issue is
<https://github.com/jwiegley/gitlib/issues/92>. The workaround was to clone
`gitlab` to `/usr/local/src/hs` and uncomment the line in `cabal.project`.

On Linux, use whatever your distro calls the above packages.
You may have to install -dev variants to get headers.

- lilypond for the lilypond backend.  This is optional.  If you never try to
compile a score via lilypond you don't need this.

- The bravura font for music symbols:
<https://github.com/steinbergmedia/bravura/releases> (the main page
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

## haskell stack

There was once support for `stack`, but I removed it when no one ever used it.
It would probably be possible to bring back without too much effort if
necessary, but cabal v2 seems to be good enough now.

## 音, Im, Synth

These are all names for the offline synthesizer.  It requires a bunch of extra
dependencies.  If you did the nix way, then you'll already have them.  The OS X
install list above also incluse them.

Otherwise, here are old docs for installing by hand, the non-nix way:

First you need more non-haskell dependencies.  Get the -dev versions as usual:

- faust - Faust had major stdlib changes a few years back, and if you use a
conservative distro, the bundled one may be too old.  Install by hand to be
sure.  I'm using `2.5.34`.

- libsamplerate - I use a local fork, with support for saving and restoring
state
    - cd /usr/local/src
    - git clone https://github.com/elaforge/libsamplerate
    - cd libsamplerate
    - git checkout save-state
    - ./autogen.sh && ./configure && make
    - Don't `make install`!  The shakefile will link directly to it.
    Otherwise it could replace a standard libsamplerate in /usr/local/lib.

    If you cloned someplace other than `/usr/local/src/libsamplerate`, you'll
need to update Local/ShakeConfig.hs update the `libsamplerate` field with
the link and compile flags.

- libsndfile - Use your package manager.

## Misc

`tools/run_profile` expects `ps2pdf` in the path, which is part of ghostscript.
It's fine if it's not, but you'll get ps instead of pdf.  Help with heap
profiling would be very welcome!

`pandoc` is used to convert `.md` to `.html` for docs, but it's big and
complicated.  Don't need to install if you just read the md files.
