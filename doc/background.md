This has original motivation, general purpose philosophical arm-waving, and
other somewhat irrelevant detritus.

### motivation

I was motivated to create a music sequencer because of what I saw as a gap
in the capabilities of existing software.

As I see it, there are basically three categories of software systems for
music writing: the mainstream MIDI sequencer or DAW (examples are Cubase,
Logic, Ardour), staff notation programs (Finale, Sibelius, Lilypond), and
language-oriented systems (csound, CLM, Supercollider).  The DAWs and staff
notation programs are standardized, monolithic, and more or less equivalent
to each other (lilypond is the exception, also being in the language camp),
but the the language systems are much more variable.  Some focus on sound
synthesis, some on score representation, and some on real-time synthesis or
note production.  Especially this last category has become very popular
recently, examples are Tidal, Extempore, overtone and leipzig in clojure,
etc.

At the time, I was involved with Javanese and Balinese music, and none of
the above categories seemed suitable.  The mainstream DAWs only really
support 12 tone equal tempered tuning, and anything outside that is
distinctly second class---MIDI synthesizers can be retuned, but it's
awkward, not standardized, and the sequencer doesn't understand it.  Beyond
that, these kinds of music (as it seems are most kinds of music around the
world) are based on a few core melodies from which other parts are derived
with greater or lesser freedom.  A DAW of course doesn't understand any of
this, and since they are monolithic and not programmable, can't be made to
understand any of it.

The notation programs are also out.  Primarily because I'm hoping to generate
sound in addition to scores, but also staff notation is just not suitable.  It
can be forced, but it's awkward at best, omitting or obscuring important
details and cluttering the page with irrelevant or misleading ones.
The point of this kind of notation is to give to a musician who will understand
it, so the further you deviate from standard practice the less useful it
becomes.

The various music programming languages could theoretically support the scales
and proper relationships of core melody to derived parts.  In practice they
don't.  They give you a blank piece of paper and the means to define your own,
which is not much better than starting from scratch.  For instance, some will
claim that they support "all scales", when they really mean you can give pitch
names arbitrary frequencies and do some modular arithmetic on them.  Real
scales are a bit more complicated than that, and reach deep into the music
theory of whatever tradition they're embedded in.  But the main issue with the
languages is that they're text-only, generally expect you to edit with a
standard text editor, and have no special support for editing notes.  What this
means is entering note lists by hand, and writing times and durations as
numbers.  It's very hard to write and read that way, especially if there are
multiple parts which are aligned in time.  To be sure it can be done, and you
can look at a large lilypond score to see how it can work, but there's a reason
that almost every kind of historical notation represents time spatially.  So
it's also no coincidence that the various languages tend to be used for
algorithmic music or sound design, neither of which need note lists.

Later I found that every other kind of music I encountered was not expressible,
for all the same reasons.  But even for equal tempered European-style music,
mainstream sequencers are not very good.  Writing for small ensembles using
physical models, even the very modest amount of breath pressure data would
quickly overwhelm Cubase's capabilities, leading to laggy editing.  The amount
of data is tiny compared to audio, so this is just that the program was not
designed to expect any significant amount of control data.  And aside from the
performance problems, the editing is primitive.  You can draw curves by hand
(if your hand is steady), or enter basic shapes, and align them by zooming in
and squinting.  Editing pitches and durations is also low level and imprecise.
The design clearly expects you to record a live performance, with minimal
editing afterwards.  So if you have a MIDI keyboard, and are a skilled
keyboardist, you can record keyboard music, or if you have a wind controller
and can play it well, you can record woodwind music.  Added to that is the fact
that quite a lot of European-style music is not actually equal tempered and the
situation seems hopeless.

Of course what people do in practice is that they mostly don't try to do those
things.  They develop new kinds of music for computers and electronics, which
make use of their strengths and de-emphasize the weaknesses above.  This is the
same sort of thing that happens with every other instrument throughout history,
so it's completely reasonable.  But, in my eyes, it doesn't live up to that
Bacon "we also have sound-houses" promise that computers should be able to
fulfill.


### implementation overview

So much for the grandiose motivation.

TODO - karya overview, how it attacks the problem


### description

At the lowest level, below any of the graphical layout, Karya implements some
ideas from the Nyquist language as a Haskell library.  Nyquist operates at the
sound level, and Karya transposes that "up a level", to produce Note records
(these are essentially a start time, a duration, and a set of control signals),
instead of samples, but otherwise the ideas are similar.  Nyquist is described
in detail at <https://www.cs.cmu.edu/~music/nyquist/> or its articles in the
CMJ vol 21 #3.  I read those back in 1997 as a high school student!

The short version is that Nyquist has a concept it calls "behavioural
abstraction."  A note is a function that returns a sound, and instead of
changing the volume or transposition or any number of other parameters by
modifying the output of the parameter, you set values in a dynamically scoped
environment.  The note function is then free to interpret those values as its
sees fit.  One that implements an oscillator could map volume to amplitude, one
that implements a bowed string model could map it to bow pressure, one that
implements a string section could map it numbers of doubled instruments, and
one that implements an entire score could leave the interpretation to the
various sections, phrases, instruments, notes, and oscillators, all the way
down the stack.  This concept is also extended to the treatment of time, which
is implemented as a function from score time to real time.  So a note is given
a starting time and duration by shifting and stretching the function.  Similar
to the volume control, it can interpret that as it sees fit.  For instance,
grace notes may give themselves a constant duration, while a trill may change
its speed or add cycles at a constant speed in order to fill the time given.
This works becasue any bit of code has access to the score->real function to
place itself, but also the inverse real->score function so it can calculate the
score equivalent of 1 second of real time at a given score location.

Since all of these parameters are passed implicitly in a dynamic environment,
even if a bit of notation doesn't understand a particular value, it will still
pass it to its dynamically scoped children, which may.  For instance, you can
wrap a whole section in a `pizz` request, and instruments that understand that
may change their behaviour.

Also, since everything is uniformly represented as a sound, it has the closure
property.  What this means in practice is that a note is syntactically the same
as a phrase, which is the same as a whole piece, and they can all be
transformed by the same functions.  To continue the example above, a `pizz`
marker goes as easy on a movement as it does on a note, or a triplet can be
formed from notes or phrases or other triplets.

#### code vs data

Since the score is simply an executable program, on the axis between code and
data this is a fundamentally code-oriented representation of notation.  Code
vs. data is a complicated trade-off, but the short version is that code is
"opaque": flexible but not easily inspected, while data is "transparent":
easily inspected and analyzed but generally not extensible.  For a general
purpose notation, I think the flexibility is worth the hassle.[^1]  Haskore,
and its descendent Euterpea <http://haskell.cs.yale.edu/euterpea/> is an
example of a system on the data side, and from the beginning has been built on
a single Music data type.  It derives much of its power from the ability to
analyze and manipulate Music, but Music itself is not extensible.  For
instance, from the beginning pitch has been an integer interpreted as an equal
tempered pitch, ornaments are limited to a hardcoded list, and even instruments
are limited to the General MIDI list of 127.  Of course it's not a simple
binary---Euterpea can and has added various means of extension, such as a
CustomInstument escape hatch, or generalizing Music with a type parameter, but
there's no free lunch, and the power of analysis decreases as the
generalization increases.

Karya itself has data-oriented sublanguages (notably for Carnatic
[solkattu](#solkattu), but these are for specific kinds of notation, and all
render down to the common notation.  But even in a very restricted domain it's
hard to design a data type that encompasses everything you may want to write!

[^1]: For an example of where code is a hassle, I sometimes need to find the
pitch of neighboring notes.  But since pitches are essentially functions, and
notes are functions that produce data with pitches inside, I have to evaluate
the next note to get its pitch.  Not only can this possibly upset requirements
about evaluation order (which I have controlled very tightly, but still are
present), but can lead to a circular dependency.  The root of the problem is
that asking the next note to evaluate itself so I can get the pitch out is much
more general than just asking for the pitch... so I have to invent a mechanism
to just ask for the pitch.  Compare this to a data oriented representation,
where you just look at the next note and look at its pitch.  However, my code
oriented pitches support a lot of fancy things, such as retuning dynamically in
time or according to context.  While a sufficiently fancy data type could also
express that, as it increases in complexity it also becomes harder to work
with, taking on the same problems as the code one.  The interesting thing is
that as the data type increases in complexity it approaches a function, but on
the other side, as the function accumulates ways to ask for specific things
(like pitch), it increasingly resembles the data type.  In the limit in both
directions, you wind up with a memo table.  There really is no free lunch, or
at least if there is one I'd like to hear about it.

#### karya's implementation

As I mentioned, Karya implements something like Nyquist's system, except
where Nyquist deals with sounds and samples, Karya deals with Note records,
which are then interpreted by a backend to become either MIDI, or Lilypond,
or control signals for FAUST instruments, or whatever else.

In many ways, Haskell is a more suitable host language for Nyquist's ideas
than its original XLisp implementation.

- Since XLisp is eager, Dannenburg had to add an ad-hoc lazily evaluated
`sound` type.  Haskell of course is pervasively lazy, so Nyquist's `sound`
could be implemented as a list of audio chunks, with no language extension
required.  Since the Haskell world has been dealing with lazy evaluation for
its entire existence, it has evolved sophisticated ways of treating strictness
and laziness.  For instance, there is an "iteratee" abstraction (whose modern
incarnation is the `pipes` and `conduits` libraries) to allow lazy streaming
and side-effects to coexist in a disciplined way---in practice, this is how
analogous sound types are implemented.  In addition to providing guarantees
about resource acquisition and release, they can statically eliminate a common
problem with lazy data that Nyquist also suffers from: what happens when you
accidentally hold on to the head of the lazy list?  Also, Haskell has been a
benefited from a lot of optimization work, such as deforestation / array fusion
techniques, which optimize away the intermediate data structures while
streaming.

    Of course Karya is working at the note level, not the sample level, and in
2017, not late 90s, so efficiency is not such a concern, but it's still
useful to evaluate a score incrementally.

- Similarly due to XLisp's strictness, Nyquist has to use macros for
"behavioural abstraction" functions which merely change the dynamic
environment.  This leads to an awkward situation where sounds are only semi
first class, in that if you assign them to a variable they lose their ability
to respond to dynamic variables.  And of course macros themselves are not first
class, so you can't pass them to other functions.  In addition, because Nyquist
reuses XLisp's dynamic variables, it has to resort to some low-level hacks to
capture closed-over variables.  For instance, Nyquist's SEQ macro has to
explicitly EVAL its code save and restore the dynamic environment, using
internal XLisp hooks.

    In Haskell, all of these problems are avoided by the `Reader` monad, which
expresses the idea of a dynamic environment.  Monadic values are explicitly
sequenced with the bind (>>=) operator, so there is no need to control
evaluation time, so no macros are needed.  The Karya equivalent of Nyquist's
SEQ is the monoid append operator, and requires no special language support.
Being a monoid, it inherits the convenient algebraic properties of an
identity element and associativity, and comes with some algebraic laws that
not only aid the programmer, but can be exploited for optimization.  For
instance, I can freely optimize away the identity value, and since I know
monoids are associative, I could freely parallelize score evaluation.

- And of course, Haskell is a widely adopted general purpose language, with all
the comforts of civilization: type checking, a proper compiler, testing
and profiling libraries, a large package database, etc.

#### orchestra and score

Another thing Karya inherits from Nyquist is that it has a single unified
evaluation step, without an orchestra / score phase distinction.

The "orchestra / score" terminology comes from CSound, which has two separate
languages, one to describe the instruments and one to drive them.  More
importantly, it also enforces two separate phases, analogous to compile time
vs. runtime.  This means that score level notation can't talk about samples,
so e.g. while you could talk about reversing notes, you couldn't reverse the
actual sounds of the notes.  Even something simple like adding reverb
requires awkward hacks.  Modern CSound has various ways to work around this,
so it's not a hard distinction, but the phase division is very much alive in
not just CSound, but more modern systems like supercollider or PD, and of
course standalone synthesizers and MIDI are yet another manifestation.

For example, in most systems the relatively simple request to put a certain
kind of reverb on a single note would require setting up an orchestra level
reverb, configuring a special channel for it, then configuring the score so
only that one note goes on that channel, and then coordinating the
communication so the score turns all the right knobs on the right reverb.
Lots of faffing about with plumbing MIDI CC numbers and audio channels and
what have you.  Wanting to customize the reverb for each note individually
would be crazy and no one would do that... and yet, purely score level
transformations like transposing each note differently are completely
reasonable.  So while you can easily express some music with different
transpositions, e.g.  `transpose +1 phrase + transpose +2 phrase`,
good luck interleaving score level and sample level, e.g.
`reverse (transpose +1 (reverb phrase))`.

In the Nyquist language, a "unit generator" is just a function that returns a
sound.

Since Karya works entirely at the score level, not sound, it seems silly to
talk about it not having an orchestra / score distinction, so it's only really
true by analogy, if we lift sound samples up to notes and notes up to high
level score notation.  And in fact this score / note distinction exists all
over again in many scoring systems.  For example, Haskore has one language for
describing notation, and a separate system of "players" which render the nested
score notation down to a flat sequence of notes.  Similar to the way a CSound
score can't work with sample level synthesis, the note level output of a
Haskore player can't become notation again.  In the same way that CSound is a
one way notes -> sound -> speaker pipeline, Haskore is a one way trip: score ->
notes -> sound -> speaker.  I don't know if there's a standard name for this
distinction, so I'll call it a "two phase" system.

So for instance, if you have two separate players that realize staccato in
different ways, you would have to put a special key in the score saying which
player to use at which point, and then build logic into the players to swap
out when they see the key.  This is the equivalent of all of that MIDI CC and
audio channel plumbing hassle.

Karya, on the other hand, can interleave realization and score, and does so
extensively.  For instance, notation for the gangsa section of a Balinese gong
is usually written as one part, but in practice will divided into separate
sangsih and polos parts, played on separate instruments.  But there are many
ways to do that, whether playing in unison, at a fixed offset (kempyung), or
interlocking, and of course there are multiple techniques for interlocking.  I
can write as score notation, at any level of from individual notes to entire
sections.

That's not to say the problem is "solved," by any means.

There is a fundamental tension between nested symbolic score, and linear
notes, in that some things can only be expressed at one level or another.
For instance, *ngoret* is a grace note linking two pitches, whose pitch depends
on its neighbors, and also implies a certain damping technique that usually
winds up lengthening the previous note.  So while the notation exists at the
score level, and the timing is possibly defined in score time, the pitch is
dependent on both the previous and following pitches.  Not only would
figuring out the next pitch at the score level require tricky out of order
evaluation, but since the same bit of score can appear in multiple contexts,
there may be multiple possible previous or next pitches.  So clearly the
score level has to figure out the timing, but can't know what the pitches are
yet.  My solution is to have the ngoret score notation emit a note with the
right timing, but with a flag saying to infer the pitch, and a separate
`realize-ngoret` notation which post-processes the notes as a stream, at
which point it can infer the right pitch, and also modify the length of the
previous note.  Also, once we're working with a linear stream of notes, the
score-level information about what is the previous or next note has been
flattened away, which means I have to define a notion of "hand" or "voice" at
the score level and annotate the notes accordingly.  This has become a
general pattern of (notation, realize) pairs.

Of course this is more or less the same plumbing hassle described above, and
it causes the same problems: forget `realize-ngoret` or put it at the wrong
level and things won't work right, or forget or mess up `hand` or `voice`
annotations and cross-block (TODO) ngoret doesn't realize correctly.  Also,
there are order dependencies between the various realize calls.  For
instance, a realize that cancels extraneous notes must run before the one
that infers things based on neighbors, and one that moves notes around for
expressive purposes must run after one which relies on looking for
score-level simultaneous notes.

So the phase distinction is still alive and well.  Some aspects of this seem
to be inherent.  For example, since I'm expressing an instrumental technique
that references previous and next pitches, that means it naturally requires
those things to be defined, which imposes restrictions on the notation.  The
fact that my particular implementation is a bit awkward and error prone is
probably a consequence of the fact that I'm using general purpose low level
notation for what is really an instrument-specific idiom.  That in turn is
an instance of the general compromise between general purpose and somewhat
unsuitable notation, and specific notation that is not reusable across
instruments and genres.

I don't think I can actually eliminate this problem, only mitigate it.  I
mitigate with naming conventions (the realize calls are always named
`realize-something`), pre-composed sets of realize calls to avoid the ordering
problem, and a general mechanism for calls to store dynamically-typed arguments
in a score event: 'Derive.Call.Post.make_delayed'.  The latter hints that this
is also yet another facet of the general problem of [evaluation
order](#evaluation-order).  The conventions and library support make it easier
to write these kinds of ornaments, but they're not eliminating the complexity,
just making it easier to express.

So if I have many of the same hassles as a two-phase system, what does
integrating processing and score processing actually buy me?


#### evaluation order

#### block notation

problems with it

### tracklang

The score is effectively a music programming language.  It's an unusual
one due to its unique requirements.  The most prominent unusual feature
is that it has 2D structure.  Most expressions are attached to events, which
have a starting time and duration.  Non-overlapping events are arranged into
tracks, and a set of tracks form a block.  Not only 

Since the vertical and horizontal dimensions are taken by time and track
alignment respectively, there is a heavy emphasis on brevity.  Also due to
its nature as a music score, expressions are repeated more than they would be
in a typical programming language, which also drives toward brevity.

It's also much more restricted than a normal language.  Every toplevel
expression returns either a stream of `Score.Event's, or control signal
samples, or pitch signal samples, but the returned events themselves are not
first class.  This lets me have separate namespaces for functions based on
return type, which is also in service of brevity.  Also this kind of
restriction makes many type errors impossible to write: since a note track
establishes its own namespace where only note calls are in scope, you can
return a signal on a note track.

There are effectively
three kinds of function call: 

The basic expression syntax a function call followed by arguments separated
by spaces: `f x y z`.  The syntax is such that the first word can contain
any character except space, which means that `1 x y` is a call to the `1`
function.

dynamic environment and arg defaulting: also for brevity
This leads to an usual way of writing named arguments: `arg=val | f`


### implementation

Call signature and Applicative.

Improvement over nyquist: dynamic environment is naturally suited to Reader.
The monadic distinction between evaluation and binding gets rid of the
awkward nyquist macros.

### music

#### low level

Behavioural abstraction -> reader monad.


### unsolved or poorly solved problems

#### Signals.

Individual samples with linear interpolation is too low level, e.g. have to
remember to explicitly emit start and end samples.  Also I'd like to be able
to efficiently translate signals in time.  This is done in an ad-hoc way for
Warp (stretch and offset) and for Score.Event (separate control transform).
Possible solutions:

- A real continuous signal, as a function.  Translation is efficient:
((+x) .), scaling is just (. (*y)).  I can choose a low sampling rate for
MIDI, a high sampling rate (e.g. audio rate) for FAUST or my own synthesizer.
Things I'm not sure about:

. Memory use?  Functions are already in normal form.  I piece together
signals from segments, presumably this would end up as a large stack of
composed functions.
. I need precise inflection points, how can I find those in the signal?  I
also need them for GUI display.
. In practice I use linear interpolation for NESS backend, and maybe others,
how can I find linear segments?  Also GUI display works well with linear
segments.

- A signal at a constant sampling rate.

. I originally considered but rejected this, due to these reasons:
. I already do this for the warp signal, since it was too complicated to try
to compose signals from linear segments.
. There are many constant signals, many which are constant per score section,
and many constant per note, they all waste a lot of space.
. I need precise times to line signal changes up exactly with note starts.
So I need a high enough sampling rate to be able to quantize note starts to
sample points.
. For MIDI most are wasted since I have to throw them away, and for a
synthesizer backend anything less than audio rate is still not enough.

- A signal of linear segments.

. Like what I have now, except the API exposes segments instead of samples,
making it harder to mess up.


- Randomized values, ControlFunction.

- Per-instrument default control, default dynamic, e.g. >inst = %dyn=.5

- Nested tempo, e.g. pulling nested tempo out.

- 1:1 score vs. hierarchical events.

There is a price to generality, why BlockResize is so complicated:
- problem of hierarchical score with arbitrary calls
- problem of generalized ruler with meter merely as a convention
(evolution of meter)
