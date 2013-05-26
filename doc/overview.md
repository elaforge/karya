<img align=left width=180 src="../../doc/img/karya.png"> Karya is a music
sequencer.  It's purely a sequencer, so it doesn't deal with actual audio at
all.  It doesn't record audio, host VSTs, or produce any kind of sound.  It
produces MIDI (there is also a Lilypond backend) and relies on external
synthesizers to turn that into sound.  There is some limited integration with
DAWs so you can combine recorded audio and written score, but Karya is
oriented around score writing, not audio.

It uses its own score format.  It's visually similar to a tracker
([Renoise](http://www.renoise.com) is a modern example), and the editing style
may be familiar to people accustomed to them.

It's main goals are to be a flexible and high level way to *hand write* scores
(so it's not oriented around realtime use), to produce the complicated control
signals needed for expressive music, and to support non-western (or
non-traditional western) idioms and scales.

As far as I know, there are no other sequencers with a similar feature set,
but if there are, I'd be interested in hearing about them.  Here are some
incomplete and biased notes on its
[relationship with other sequencers](other-sequencers.md.html).

## Music

link to music page, link to scores

## Features:

<img align=right width=180 src="../../doc/img/ss-vla.png">

- Ornaments.  There is a library of musical ornaments, such as grace notes,
tuplets, trills of various flavor, arpeggio, echo, delay, etc.  These are
called "calls", because they are analogous to function calls.  They can be
arbitrarily complicated.  For example, there are postprocessing operations that
apply instrument-specific restrictions or idioms, such as selecting a string
for a given pitch, or adding pitch sloppiness to especially rapid notes.
Another may simply put a "pizz" attribute on a note or section of score, or
slide between two pitches in a certain way.  Scores are composed
hierarchically, and there's no strict division between call that modifies or
emits a single note and one that modifies or produces an entire piece.  Calls
can take arguments and be written with custom symbols, and when the existing
ones aren't sufficient you can write your own.

- Pitch control.  You have full control over the pitch of each note, and can
apply customized forms of vibrato, portamento, vocal ornaments, or whatever
else, all within whatever scale is in use.  The same goes for other control
signals.

- Scales.  Scales can use custom symbols and can take arguments for per-note
pitch variations.  Scale pitches can depend on signals (e.g. a gradually
retuning a scale) or on other instruments (e.g. the intonation of one part is
defined relative to another).  For example, you can write the melody in
mean-tone, and define the harmony in just intervals relative to the melody.
Enharmonics and chromatic and diatonic transposition are supported for scales
that employ those concepts.  You can define your own scales that involve any
combination of the above concepts.

- Tempo control.  You can give separate tempos to separate parts
simultaneously, so you can express that one part is pushing or lagging the
tempo, or that one is swung but another is straight, or swung in a different
way.  Tempos compose, so a rubato can be composed with an accelerando for the
expected effect.  Calls can ignore tempo and play in absolute time, so a
trill can choose to either stretch with a tempo change or remain at a constant
speed and add cycles.

- Integration.  The derived output of a bit of score can be integrated back
into another score to produce derived parts.  The generated score can then be
further edited, and changes to the original source will be merged into the
modifications, to a limited degree.  For instance, a part that mirrors another
with added idiomatic ornaments plus hand tweaks, or a whole section which is
the reverse of another section, edited to sound better.

- [REPL](repl.md.html).  All non-GUI interaction is through a command-line
interface, so you can do complicated transformations by writing a function.
The language is Haskell, which the rest of the program is written in, and it
has direct access to all internal funtionality.  If you frequently use a
particular transformation you can compile it into the program by pasting it
into a file.

- Instruments.  Instruments can map MIDI CC numbers to symbolic names, support
keyswitches as named attributes, and automatically multiplex MIDI channels to
give note-level control over pitch or control signals.  They are collected
into a searchable database.  The database is created by either entering patch
information by hand, or for external synthesizers that use them you can parse
sysex messages to add to the database.

- Multiple backends.  The main backend is MIDI, and this is the most well
developed one, but there is also some ability to export to lilypond.
Lilypond export is necessarily limited because it's hard to translate between
different kinds of scores, but with a bit of work you can get
(reasonable looking output)[viola-sonata.pdf].

- Incremental saving.  The score is continually saved as changes are made.  You
can undo and make different changes, resulting in a branching history.  If
you set checkpoints along the branches you can jump between different versions
at will, and hopefully no edits need ever be lost.  The underlying storage is
git, so you can use the usual git tools to inspect and manipulate it, though
many source control concepts don't really apply.

## Weaknesses:

- Complicated.  If there is a bug in a call's implementation it can be a
hassle to track it down what is going wrong and fix it.  Due to [slicing and
inversion](slicing-inverting.md.html), the structure of the score can get
complicated and hard to understand.  There are a lot of tools for inspecting
intermediate output, but sometimes it can feel like debugging a compiler.  And
since it's a rather programmerly view of music and relies on writing code to
perform operations, it will probably never be very usable by non-programmers.

- Slow.  It relies on caching to rederive scores quickly.  If you put your
score together in a way that defeats caching it will start taking a long time
to rederive a score.  Fortunately it's relatively easy to work with the cache
system, and you get notified about cache misses so you should notice when
something goes wrong.  But the cache, as usual, is a rich source of its own
bugs.

- Greedy.  What with the various caches, an internal focus on simplicity over
efficiency, and plenty of unoptimized bits, it demands quite a lot of memory
and CPU for a program that doesn't actually handle audio.  In addition, if you
are writing your own calls you will likely need to recompile a fair amount
and that's no lightweight operation either.

- Non-realtime.  While you should be able to play back a score soon after
making changes, it's still very much oriented around non-realtime
score-writing.  You cannot change a playback in progress.  Support for
recording MIDI is present but not a focus, since it's hard to integrate a
low-level recorded performance with a high-level handwritten score.

- Complicated MIDI routing.  Since it doesn't host plugins itself you have to
route MIDI to a plugin host, which likely requires a bunch of virtual MIDI
ports.  If you route to a DAW like Reaper or Ardour you can then bounce or
"freeze" tracks by recording in the DAW, but since neither of these programs
provide much remote control ability you likely have to set up the routing
yourself.  I can do some limited integration with syncing plays and stops, but
closer integration is harder given that JACK is only supported by Ardour and
Rewire is proprietary.

- MIDI limitations. Due to the general wretchedness of MIDI, you'll need a lot
of MIDI ports and channels, and will be limited to low resolution controls, on
both value and time axes.  The underlying problem is the software synths, which
have all deeply embedded MIDI's limitations.  To solve this we'd need to drag
software synths out of the '80s, create a new high-resolution protocol, and
modify the major samplers and synthesizers to support it.  This has been tried
but failed, perhaps because there were no sequencers or input devices to
generate the high resolution output.  VST's "automation parameters" are
basically higher resolution MIDI controls, so some of this could probably be
ameliorated by extending a VST host to turn high resolution OSC into VST
automation.  Ardour is the only open source DAW so it's the only option here.
Someday I would like to write my own synthesizer that responds to OSC.

- The score format is not as efficient at displaying vertical structure (chords
and harmony) as 5-line staff notation.  This is the price of generality, since
the score format is also more flexible than staff notation.

- At the moment it's still very early in development, and has many
underdocumented and buggy parts.  It's also changing rapidly, and code written
against internal interfaces is likely to be broken by changes down the line.
And there's no defined external interface yet, so everything is an internal
interface.

## Documentation

Don't hesitate to look at [haddock documentation](../haddock/index.html) or
the [source](../hscolour/).  Since using karya probably involves writing code
that interacts with the internal APIs, you'll need to be familiar with the
code.  Sometimes the documentation isn't exported as haddock, so if the haddock
seems sparse try the "source" link.

[My blog](http://elaforge.blogspot.com/) has notes I have taken during
development.  It's mostly just notes to myself, but it may give some
historical context for why certain features are the way they are.

Karya is divided into several layers, corresponding to the top level
directories in the source tree:

- fltk - Low level C++ to draw the GUI.  Nothing much interesting here unless
you like reading C++.

- Ui - The UI level has the data structures that hold the score, all collected
in 'Ui.State.State'.  They mostly correspond directly to what is visible in the
GUI.  This is what gets saved when you save a score.

- Cmd - Cmds take user input and transform the Ui state.  This layer handles
all user interaction.

- Derive - The [deriver](derivation.md.html) interprets the Ui State and turns
into into a stream of lower level events.  It implements the "tracklang"
language described below.

- Perform - The perform layer turns Derive output into a backend specific
format, e.g. MIDI messages or lilypond score.  It's also responsible for
"playing" the output, e.g. scheduling MIDI messages with the MIDI driver or
calling lilypond.

There are a few auxiliary programs:

- LogView - Logview is a separate program that formats log output.  The karya
GUI doesn't have a place for global information, so logview serves this purpose
too.  When log messages display stack traces, you can click on them to
highlight their positions in the score.  'LogView.LogView'.

- browser (Instrument) - This is a simple browser for the instrument database.
To the UI and Derive layers, an instrument is just a text string, but they have
quite a bit of extra information associated with them which is important to the
performer.  'Instrument.Browser'.

- make_db - If the instruments involve expensive operations like parsing
directories full of sysex messages, they can serialize the parsed instruments
to disk.  What this is exactly depends on the per-synth definitions in
Local/Instrument/.  `make_db` just runs each instrument's `make_db` function.

- [repl](repl.md.html) - This connects to a running sequencer and lets you
interact at the programmatic level by sending haskell expressions to be
evaluated in the Cmd context.  `send` is a one-off version of repl that can be
used to send a single command from shell scripts.

- print_keymap - Write out a reference to the
[global keymap in HTML.](keymap.html)

## karya

For the purposes of documentation, there are three major layers:

- [UI](ui.md.html) handles user interaction, score editing, the GUI, and the
REPL.  It's divided into UI which is just the GUI part, and Cmd, which is the
infrastructure to turn user actions into score edits.

- [Derivation](derivation.md.html) is the process of converting the UI-level
score into medium-level score events.  This is where the complexity of score
interpretation is.

- [Performance](performance.md.html) converts the medium-level score events to
the low level output that can directly produce sound.  What this is depends on
the backend.  For example, the MIDI backend converts score events to MIDI
messages, while the lilypond backend converts them to a lilypond score. Unlike
derivation, there's likely nothing user-configurable here.  The idea is that
score events are basically backend-independent, but there are various hacks due
to the different capabilities of backends, and a certain amount of tangle due
to instruments.  Conceptually, an instrument is just a string at the derivation
level, but in practice they have various attributes which affect the derivation
level, e.g. a default scale, or even affect the Cmd layer by bringing custom
Cmds into scope.
