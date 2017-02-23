These modules require additional dependencies.  Enable in Shake.Config, and
install from build/enabled-deps.cabal.

This directory contains the synthesizer implementations, which are somewhat
independent of the sequencer.

The basic design is that the sequencer tells the synthesizers what to play by
writing 'Synth.Shared.Note.Note's to a file.  Then it starts the synthesizers,
which read the notes and write audio to a shared cache.  When the sequencer
wants to play, it sends a start MIDI message to a special VST which will stream
audio from the cache.  As usual, this is greatly complicated by the desire to
only recalculate the bits of audio that correspond to the parts of the score
that changed.  The cache is not actually implemented, but plans are in
`doc/dev_notes/synthesizer`.


## out of date scribble

I want to be able to compile the sequencer without the synthesizer
dependencies, and also just to avoid a hairball of cross dependencies, so there
are some rules about which imports are allowed:

- Everything in Synth.Shared.* can be imported by both seq and Synth.*

- Synth can import Perform.Pitch.  It's convenient to have NoteNumber,
and it has no further dependencies except Derive.ShowVal.  It's just because
NoteNumber has some convenient utilities.

- Synth.Sampler.PatchDb can import from seq, to define Instrument.Inst.
If this becomes a problem I have to split them up with a PatchName
key.

- Local.Instrument can import Synth.Sampler.PatchDb.

- Other than that, seq can't import from Synth.* and Synth.* can't
import from seq.  I'm not sure how to enforce this though.

This approach means that if 'Shake.Config.enableSynth' is False, I don't need
the synth packages, but I also can't compile them even if the necessary
packages are present, because I don't have the association to know that those
packages should be passed to Synth.* targets.  To implement that I'd need a
full-on per-target package system, or a way to infer packages needed by a
target.  I actually have the beginning of this in
'Shake.HsDeps.importsPackagesOf_', but pursuing this would be really
complicated, not just the shake support but also because many global things
like tests, profiles, and haddock would need similar configuration.

Just in case I'm ever tempted to try this again, here's why it's complicated:

- There should be one master set of packages and versions, and then a binary
can select a subset from it.  Ideally I could chase packages along with
modules... and I suppose I could if I have a mapping from module to package,
but I'd need to cache in a file.

- How do I know when to rebuild?  It's when the packages list changes.  So I
need to remember what the package list was, and only rebuild when it changes.
I could do it automatically by saving the packages with the cache and only
rebuild if it's changed.  The cabal file is updated when it changes, so I could
use that, but I need a global list and I want to make cabal files non-global
now.

  - So on every run, write packages to a file, if they changed.  Then cabal
    files and the package db cache need that file.
  - Then I can use this to create the cabal files, but I definitely don't want
    a cabal file for each binary.  So load the package db and use it with
    transitive deps to create a cabal file for specific targets.

- I think maybe I don't want to do this after all.  While I think it would
work, I still have to make things like tests non-global, or tests wind up with
all packages.  At that point I wonder what exactly the giant benefit is that
makes this all worth it.  It would just be to be able to build synth and karya
separately... but do I really need that so much?

- globalPackages - makeHaddock

- buildHs - for the link.  Needs union of all hsORule packages.  This just
takes the binary packages list.

- hsORule - for compile.  So it needs packages that the hs imports.
