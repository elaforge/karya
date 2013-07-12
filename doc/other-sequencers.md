## other sequencers

### mainstream

These

- Cubase - Recent versions of cubase actually support per-note articulation
and pitch, along with a fancy editor for them.  The latest version of VST3
supports per-note signals.  When using MIDI it can multiplex channels like
karya does by default.  However, it appears no one except Steinberg
writes VST3s, so in practice it's equivalent to writing your own
synthesizers.  I haven't experimented with the channel multiplexing to see
how clever it is.  It also supports keyswitches 

- logic, sonar, digital performer, ...

- Ableton Live, ...

- ardour, qtractor, non-timeline, non-sequencer - These are basically free
versions of the commercial programs.

- rosegarden

### notation

- Finale, sibelius

### composition

These are programs that are similar to karya in that they treat music in a
higher-level way.  As far as I can tell, they're all restricted to 18th
century style tonal western music.  Being specialized, they can understand
the music better, and so as they get fancier they start resembling
automatic music generation and harmonization systems.

- synfire - Commercial, very expensive.  It only supports western tonal
music, and seems basically non-extendable.  Appears to have pretty
elaborate understanding of that kind of harmony, but the website is all
marketing fluff so it's hard to tell.

- Rapid Composer - <http://www.musicdevelopments.com/>

### tracker

- Renoise - Classical tracker extended with VST support.

- Buzztrax, Beast

### language

- nyquist, csound, clm, 

- supercollider, max, reaktor - These are languages with a focus on sound
synthesis, rather than composition.  They tend to be more focused on
designing instruments rather than writing music.

- hackage temporal-music-notation, temporal-music-notation-demo,
temporal-music-notation-western

#### livecoding

Livecoding languages focus on writing a kind of score in realtime.

- chuck - <http://chuck.cs.princeton.edu/>

- overtone - <https://github.com/overtone/overtone>

- Conductive - <http://www.renickbell.net/conductive/doku.php/start>

## Relation to other sequencers

- There is a family of "academic" music programming languages: csound, clm,
supercollider, max, chuck, etc.  Being languages, they offer quite a bit of
flexibility, but they tend to be oriented around an academic style of music,
which eschews such conservative trappings as melodies, themes, harmony, and the
like.  Perhaps this self-supporting: it's hard to write a melody in text, and
if you do, it's even harder to write an expressive dynamic curve by typing in
envelope breakpoints, and being limited to a compile debug cycle means you
won't get to hear it very soon.  This makes it much easier to write generative,
algorithmic, or abstract sound-scapey stuff.  So they attract people who are
interested in that sort of thing, who further develop the field in the
direction of fractal note generators and markov sequences.  No one minds how
awkward it is to write a melody because that's not the kind of music they do.
Those things just aren't in the academic computer music idiom.

- Commercial sequencers: cubase, logic, sonar, etc.  They're designed to work
like a 4-track to not scare musicians, so they're inflexible.  Generally
speaking, you record on your keyboard, do some limited editing, and that's
that.  But on the other hand, being all about conservative trappings, they can
do melodies and harmonies and instant feedback pretty well.  But because of
their 4-track origins, they're entirely oriented around recording, and because
the only MIDI instrument that caught on is the keyboard, you wind up with
everything sounding like it's played on a keyboard.  Perhaps this is
self-supporting too: whole genres have grown up around music that sounds like
it's played on a keyboard.  So those programs attract people who want to do
that kind of music, and no one minds much how awkward it is to write in a
non-keyboard-like idiom, or something in a non 12-TET scale, or an instrument
whose part is derived from another according to complicated rules.  Those
things just aren't in the popular electronic music idiom.

- There's actually another kind of music programming language, the "livecoding"
ones, which are very good at instant feedback, and seem oriented somewhat more
in the popular electronic music direction.  But it's not because they solved
the expressiveness problem, they're moving in the direction of the more
mechanical style of popular music.
