This has original motivation and general purpose arm-waving.  The intent is
that while the other documentation has specific "how to do this" or "how does
this work" information, this is a place for bigger picture thoughts about
design, and specifically more general problems, along with solutions which seem
to fit them well, and those which don't.

### motivation

I was motivated to create a music sequencer because of what I saw as a gap
in the capabilities of existing software.

As I see it, there are basically three categories of software systems for music
writing: the mainstream MIDI sequencer or DAW (examples are Cubase, Logic,
Ardour, Ableton Live), staff notation programs (Finale, Sibelius, Lilypond),
and language-oriented systems (csound, CLM, Supercollider).  The DAWs and staff
notation programs are standardized, monolithic, and more or less equivalent to
each other (lilypond is the exception, also being in the language camp), but
the language systems are much more variable.  Some focus on sound synthesis,
some on score representation, and some on real-time synthesis or note
production.  Especially this last category ("livecoding") has become popular
recently, examples are Tidal, Extempore, Overtone.

For a long time, I've been involved with Javanese and Balinese music, and none
of the above categories seemed suitable.  The mainstream DAWs only really
support 12 tone equal tempered tuning, and anything outside that is distinctly
second class.  MIDI synthesizers can be retuned, but it's awkward, not
standardized, and the sequencer doesn't understand it.  Beyond that, these
kinds of music (as it seems are most kinds of music around the world) are based
on a few core melodies from which other parts are derived with greater or
lesser freedom.  A DAW of course doesn't understand any of this, and since they
are monolithic and not programmable, can't be made to understand any of it.

The notation programs are also out.  Primarily because I'm hoping to generate
sound in addition to (or instead of) scores, but also staff notation is just
not suitable.  It can be forced, but it's awkward at best, omitting or
obscuring important details and cluttering the page with irrelevant or
misleading ones.  The point of this kind of notation is to give to a musician
who will understand it, so the further you deviate from standard practice the
less useful it becomes.  You could even make a case that staff notation is not
that suitable for a lot of European-style music of the last hundred years or
so, let alone other kinds, but the general impossibility of spelling reform
shows how strong and valuable a widely recognized convention is.

The various music programming languages could theoretically support the scales
and proper relationships of core melody to derived parts.  In practice they
only support what the original authors wrote libraries for, which is generally
not notes and scales.  For instance, some will claim that they support "all
scales", when they really mean you can give pitch names arbitrary frequencies
and do some modular arithmetic on them.  Real scales are more complicated than
that, and reach deep into the music theory of whatever tradition they're
embedded in.  But the main issue I have with the languages is that they're
text-only, generally expect you to edit with a standard text editor, and have
no special support for editing notes.  What this means is entering note lists
by hand, and writing times and durations as numbers.  It's very hard to write
and read that way, especially if there are multiple parts which are aligned in
time.  To be sure it can be done, and you can look at a large lilypond score to
see how it can work, but there's a reason that almost every kind of historical
notation represents time spatially.  So it's also no coincidence that the
various languages tend to be used for algorithmic music or sound design,
neither of which need note lists.

Later I found that every other kind of music I encountered was not expressible,
for all the same reasons.  But even for equal tempered European-style music,
mainstream sequencers are not very good.  Writing for small ensembles using
physical models driven by the Cubase sequencer, even the very modest amount of
breath pressure data would quickly overwhelm Cubase's capabilities, leading to
laggy editing.  The amount of data is tiny compared to audio, so the problem is
merely that the program was not designed to expect any significant amount of
control data.  Aside from the performance problems, the editing is primitive.
You can draw curves by hand (if your hand is steady), or enter basic shapes,
and align them by zooming in and squinting.  Editing pitches and durations is
also low level and imprecise.  The design clearly expects you to record a live
performance, with minimal editing afterwards.  So if you have a MIDI keyboard,
and are a skilled keyboardist, you can record keyboard music, or if you have a
wind controller and can play it well, you can record woodwind music.  Added to
that is the fact that quite a lot of European-style music is not actually equal
tempered and the situation seems hopeless.[^1]

Of course what people do in practice is that they mostly don't try to do those
things.  They develop new kinds of music for computers and electronics, which
make use of their strengths and de-emphasize the weaknesses above.  This is the
same sort of thing that happens with every other instrument throughout history,
so it's completely reasonable.  But, in my eyes, it doesn't live up to that
Bacon "we also have sound-houses" promise that computers should be able to
fulfill.

[^1]: As an aside, the fact that Cubase later added a "note expression" feature
which allows per-note curves and provide some editing features shows that they
are at least aware of the problem, and put some significant work into a
solution.  I don't have experience with that feature since I gave up on Cubase
and started on my own thing before that version came out, but my impression is
that it can't help but be manual fiddling with curves, even if the specific
manual tools are more sophisticated, because they would never give mainstream
users a whole programmable system.  My feeling is that anything based on
manually tweaking curves in a GUI is going to be too low level to scale, or at
least will be so much work that the curves will be relegated to ad-hoc special
effects, not pervasive techniques privileged at the same level as the pitches
and durations of the notes themselves.


### relationship with Nyquist

At the lowest level, below any of the graphical layout, Karya implements some
ideas from the Nyquist language as a Haskell library.  Nyquist operates at the
sound level, in that its "notes" are functions that emit streams of samples.
Karya transposes that up a level, in that its notes are functions that emits
streams of Note records, but aside from that, the ideas are similar.  Nyquist
is described in detail at <https://www.cs.cmu.edu/~music/nyquist/> or its
articles in the CMJ vol 21 #3.

The Karya specific Note (with a capital N to refer to that specific data type)
is in essence a triple of a start time, duration, and a mapping from symbolic
control names to continuous functions.  I use "note" with a small "n" to refer
to a the functions that return Notes.  Sometimes I use the word "notation",
meaning the same thing.  The important thing to remember is that these all
refer to a plain function which takes an environment structure (described
below) to a stream of Notes.  When I refer to a "performer", or a "backend", I
mean the next stage in the process, which will be MIDI and the MIDI scheduler,
or a Lilypond code generator, or a standalone FAUST synthesizer.

Nyquist has a concept it calls "behavioural abstraction."  In Nyquist, A note
is a function that returns a sound, and instead of changing the volume or
transposition modifying the output of the parameter, you set values in a
dynamically scoped environment.  The note function is then free to interpret
those values as it sees fit.  One that implements an oscillator could map
volume to amplitude, one that implements a bowed string model could map it to
bow pressure, one that implements a string section could map it numbers of
doubled instruments, and one that implements an entire score could leave the
interpretation to the various sections, phrases, instruments, notes, and
oscillators, all the way down the stack.  This concept is also extended to the
treatment of time, which is implemented as a function from score time to real
time.  So a note is given a starting time and duration by shifting and
stretching the function.  Similar to the volume control, it can interpret that
as it sees fit.  For instance, grace notes may give themselves a constant
duration, while a trill may change its speed or add cycles at a constant speed
in order to fill the time given.  This works becasue any bit of code has access
to the score->real function to place itself, but also the inverse real->score
function so it can calculate the score equivalent of 1 second of real time at a
given score location.

In Karya, I extended Nyquist's notion of a dynamic environment to the namespace
of functions available.  This means that an instrument can modify the function
namespace to insert its specific kinds of notation, or override existing
functions with conventional names, such as a more instrumentally-appropriate
"tr" trill.  In fact, at the score level, that's all an instrument is, just a
thing that brings certain functions into scope.  In the same way, a scale is a
thing that brings pitch-producing functions into scope.  Of course there is
also notation that modifies the meaning of notation under it.  An example of
where this comes up is some notation may want to redefine what a "plain note"
is, to change the behaviour of nested notation.  But if its new plain note
itself wants to generate some plain notes, it will have to restore the original
definition to avoid infinite regress!

Since all of these parameters are passed implicitly in a dynamic environment,
even if a bit of notation doesn't understand a particular value, it will still
pass it to its dynamically scoped children, which may.  For instance, you can
wrap a whole section in a `pizz` request, and instruments that understand it
will change their behaviour... hopefully in a way that the composer will agree
is consistent with the word "pizz"!

The only thing special about the "note" functions that make up a score as
opposed to any other kind of function is that they all take the same argument
type, a dynamic environment record, and all return the same type of
result---sounds for Nyquist, while Karya implements three families: ones that
return Notes, ones that return signal samples, and ones that return pitch
signal samples.  This uniformity means that they have the closure property.
What this means in practice is that a note is syntactically the same as a
phrase, which is the same as a whole piece, and they can all be transformed by
the same functions.  To continue the example above, a `pizz` marker can be
applied to a note or an entire movement, or a triplet can be formed from notes
or phrases or other triplets, all nested arbitrarily.

As implied above with the `pizz` example, beyond the basic mechanics of calling
note functions and merging their results, all of the various ways to control
them are essentially symbolic and conventional.  That is, there is nothing
special about a "pizz", except that it is a symbol that you might put into the
environment, and what anyone does when they see it is just down to what they
choose to do.  I think this lack of a direct link between a name for a
behaviour and the behaviour itself is essential for a generalized notation.  It
ensures that the vocabulary is open (we don't want to be stuck with a hardcoded
list of articulations), and that the result it implies is also open (music and
instruments are so variable that we don't want to be stuck with having to
always interpret that symbol as the same action).  The downside is that if
you write "pazz" on accident, no one can tell you for sure that it's nonsense,
or if two instruments interpret "pizz" in confusingly different ways, then you
may get confused by the result.  In practice, this latter form of confusion
arises from the name of functions themselves, e.g. one convention may decide
that an "m" annotation means "play muted", while a drum might already have a
stroke named "m".


#### code vs data

Since the score is simply an executable program, on the axis between code and
data this is a fundamentally code-oriented representation of notation.  The
essence of this is that code is "opaque": flexible but not easily inspected,
while data is "transparent": easily inspected and analyzed but generally not
extensible.  This is a fundamental trade-off, in that the opacity of a function
gives it its implementation flexibility.  For a general purpose notation, I
think the flexibility is worth the hassle.[^2]  Haskore, and its descendent
Euterpea <http://haskell.cs.yale.edu/euterpea/> is an example of a system on
the data side, and from the beginning has been built on a single Music data
type.  It derives much of its power from the ability to analyze and manipulate
Music values, but Music itself is not extensible.  For instance, from the
beginning pitch has been an integer interpreted as an equal tempered pitch,
ornaments are limited to a hardcoded list, and even instruments are limited to
the General MIDI list of 127.  Of course code and data is not a binary
distinction.  Euterpea can and has added various means of extension, such as a
CustomInstument escape hatch, or generalizing Music with a type parameter.  But
there's no free lunch, and the power of analysis decreases as the
generalization increases.

The `music-suite` library (<https://github.com/music-suite>) is an example of a
system similar in spirit to Haskore but emphasizing generality and extensible
data structures.  It's somewhat generalized beyond my ability to understand it,
but for example, at the bottom pitch still reduces down to (pitch class,
sharps/flats, octave), which is not able to represent the pengumbang / pengisep
distinction in Balinese scales, and is ambiguous for Carnatic scales with
differing arohana and avarohana.

Karya itself has data-oriented sublanguages (notably one oriented around
rhythmic structures, specialized to notate Carnatic solkattu), but these are
for specific kinds of notation, and all render down to the common notation.
But even in a restricted domain it's hard to design a data type that
encompasses everything you may want to write!

[^2]: For an example of where code is a hassle, I sometimes need to find the
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
problem with lazy data that Nyquist also suffers from: if you accidentally hold
on to the head of the lazy list, you have a memory leak, known as "drag".
Also, Haskell has been a benefited from a lot of optimization work, such as
deforestation / array fusion techniques, which optimize away the intermediate
data structures while streaming.  To be clear, Karya doesn't actually use a
lazy streaming abstraction for its Notes, other than a plain list, but it
could if drag became a problem.

    Of course Karya is working at the Note level, not the sample level, and in
2017, not late 90s, so efficiency is not such a concern, but it's still useful
to evaluate a score incrementally.  It's interesting to note, though, that
Karya can't use laziness to the same extent as Nyquist, because it
intentionally keeps many intermediate Note streams in memory as part of the
caching mechanism, and it evaluates the score aggresively as opposed to
on-demand to get global error analysis.  Still, the underlying feature which
enables laziness is the absence of data dependencies between different parts of
the score, and this is valuable on its own just for understandability, in
addition to enabling parallelism.

- Due to XLisp's strictness, Nyquist has to use macros for "behavioural
abstraction" functions which merely change the dynamic environment.  This leads
to an awkward situation where sounds are only semi first class, in that if you
assign them to a variable they lose their ability to respond to dynamic
variables.  And of course macros themselves are not first class, so you can't
pass them to other functions.  In addition, because Nyquist reuses XLisp's
dynamic variables, it has to resort to some low-level hacks to capture
closed-over variables.  For instance, Nyquist's SEQ macro has to explicitly
EVAL its code save and restore the dynamic environment, using internal XLisp
hooks.

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
the comforts of civilization: type checking, a proper compiler, testing and
profiling libraries, a large package database, etc.  Don't underestimate the
importance of powerful test infrastructure like QuickCheck, profiling and
benchmarking infrastructure including robust statistical analysis like
Criterion, and even a documentation system like Haddock.

#### orchestra and score

Another thing Karya inherits from Nyquist is that it has a single unified
evaluation step, without an orchestra / score phase distinction.

In the discussion below, I'll talk about an orchestra or score phase, or
score time vs. real time, or score vs. player.  It's important to note that
this "real time" or "player" phase is *not* the same as the "performer" I
mentioned above.  In Karya, the performer is yet another phase, which occurs
later, and is not part of the discussion in this section.

The "orchestra / score" terminology comes from csound (and perhaps the "Music
N" family before that), which has two separate languages, one to describe the
instruments and one to drive them.  More importantly, it also strictly enforces
two separate phases, analogous to compile time vs. runtime.  This means that
score level notation can't talk about samples, so e.g. while you could talk
about reversing notes, you couldn't reverse the actual sounds that the notes
produce.  Even something simple like adding reverb requires awkward hacks.
Modern csound has various ways to work around this, so it's not a hard
distinction, but the phase division is very much alive in not just csound, but
more modern systems like supercollider or PD, and of course standalone
synthesizers and MIDI are yet another manifestation.

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

Since Karya works entirely at the score level, not sound, it seems silly to
talk about it not having an orchestra / score distinction.  It's only really
true by analogy, if we lift sound samples up to notes and notes up to high
level score notation.  And in fact this score / note distinction exists all
over again in many scoring systems.  For example, Haskore has one language for
describing notation, and a separate system of "players" which render the nested
score notation down to a flat sequence of notes.  Similar to the way a csound
score can't work with sample level synthesis, the note level output of a
Haskore player can't become notation again.  So in the same way that csound is
a one way notes -> sound -> speaker pipeline, Haskore is a one way trip: score
-> notes -> sound -> speaker.  I don't know if there's a standard name for this
distinction, so I'll call it a "two phase" system.  Of course there are more
than just two phases involved in the whole journey from score to sound, but I'm
only talking about the top two.

For instance, if you have two separate players that realize staccato in
different ways, you would have to put a special key in the score saying which
player to use at which point, and then build logic into the players to swap out
when they see the key.  This is the equivalent of all of that MIDI CC and audio
channel plumbing hassle.

Karya, on the other hand, can interleave realization and score, and does so
extensively.  In Karya, a function transforming notation (which, remember, is
itself just a function returning Notes) can call its transformee with an
altered environment (which corresponds to passing different arguments to it),
or it can evaluate the transformee to the Notes stream, and transform that
stream (I call these "postproc"s in the general case, since they are a kind of
post-processing on the note stream, but I call them "realize calls" for the
specific examples below).  The former corresponds to a score transformation in
any music language, the latter corresponds to directly transforming the samples
produced by the score.  A delay implemented with the former technique will
happen in score time, in that the delayed music will conform to whatever tempo
changes are in scope.  A delay implemented with the latter technique will be
"flat", in that it will delay a certain number of seconds regardless of the
tempo.  A two-phase system like Haskore would be able to do the same thing, but
the flat time delay would have to be at the player level, so you couldn't then
integrate that into other score-level constructions, so it couldn't be part of
a phrase then used as an argument to something which builds a larger structure.

However, it turns out that the Karya implementation for more complicated kinds
of notation begins to resemble the two-phase approach.  I'll illustrate with an
example:

There is a fundamental tension between nested symbolic score, and linear notes,
in that some things can only be expressed at one level or another.  For
instance, *ngoret* is a grace note linking two pitches, whose pitch depends on
its neighbors, and also implies a certain damping technique that usually winds
up lengthening the previous note.  So while the notation exists at the score
level, and the timing is possibly defined in score time, the pitch is dependent
on both the previous and following pitches.  Not only would figuring out the
next pitch at the score level require tricky out of order evaluation, but since
the same bit of score can appear in multiple contexts, there may be multiple
possible previous or next pitches.  Consider the last note before a repeat: you
can only know the next note once you have realized the repeat.  So this is
a problem where some bits of information are only available at score time, and
some are only available at the Note level.

My solution is to split ngoret into two functions.  The score level notation
will emit a note with the right timing, but with a flag saying to infer the
pitch, and to possibly modify the length of the previous note.  This in turn
relies on a separate `realize-ngoret` function which post-processes the notes
as a stream, at which point it can infer the right pitch, and also modify the
length of the previous note.  One wrinkle is that once we're working with a
linear stream of notes, the score-level information about what is the previous
or next note has been flattened away, which means I have to define a notion of
"hand" or "voice" at the score level and annotate the notes accordingly.  This
is a restriction, but it exposes a fundamental assumption of the instrument
that the notation depends on: that each note belongs to a certain hand, and
that one hand cannot play two notes simultaneously.  In fact, all notation that
depends on a previous or next note has this limitation.  Anyway, this specific
solution for ngoret notation has become a become a general pattern of
(notation, realize) pairs.

You might be recognizing this as the two-phase split I was decrying above.
Indeed it is, accompanied with the same plumbing hassle, and the same kinds of
problems.  For example, if you forget `realize-ngoret`, then things won't work
right, or if you mess up the `hand` annotations the wrong pitches get inferred.
More seriously, as I accumulate more kinds of notation, the various `realize`
calls are not necessarily commutative, which is to say you have to put them in
the right order.  For instance, a realize that cancels extraneous notes must
run before the one that infers things based on neighbors, and one that moves
notes around for expressive purposes must run after one which relies on looking
for score-level simultaneous notes.

So the phase distinction is still alive and well.  Some aspects of this seem to
be inherent.  For example, since I'm expressing an instrumental technique that
references neighboring pitches, that means it naturally requires those things
to be defined, which imposes restrictions on the notation.  A more specialized
notation could eliminate the possibility of messing up hand annotations by
syntactically allowing only two monophonic lines per instrument, and eliminate
the `realize` order dependency simply by hardcoding them at the top level...
and at this point we've arrived back at the explicitly two-phase Haskore style.
I have yet to locate my long-awaited free lunch.

Since one of my goals is to have a general notation that can reused across
styles, and instrumental techniques that assume as little as possible about the
instrument for the same kind of reuse, it seems I'm resigned to a somewhat
awkward notation and somewhat error-prone usage.  I do implement ad-hoc
techniques to mitigate it, such as naming conventions (the realize calls are
always named `realize-something`), pre-composed sets of realize calls to avoid
the ordering problem, and a general mechanism for calls to store
dynamically-typed arguments in a score event: 'Derive.Call.Post.make_delayed'.
The latter hints that this is also yet another facet of the general problem of
evaluation order, but that's another complicated subject.  The conventions and
library support make it easier to write these kinds of ornaments, but they're
not eliminating the complexity, just making it easier to express.

So I think the conclusion is that having an integrated Nyquist-like system
does make for more expressive power.  It works just fine for simple things like
delay.  However, as soon as notation gets even a little bit complicated it
acquires aspects that require logic at both score time and performance time,
which in turn requires careful interleaving.  I think future work might not
be able to eliminate those requirements, but perhaps it could place it in a
more disciplined structure.  For instance, I could imagine trying to address
the non-commutativity of `realize` functions directly, with a means to describe
the their dependencies.

The compiler level analogy for all of this is compile-time and runtime, and in
fact compiler optimization phases are notoriously order dependent and tricky.
Usually of course they are precomposed and hardcoded, but for instance GHC
admits limited extensibility via its rewrite rules.  They come with a numeric
phase system which of course is brittle and awkward.  LLVM also exposes a
configurable set of optimizations, so they may have some framework to keep them
under control, but it seems that the general problem is really hard.  Lisps in
general and Racket in particular have explored staged compilation.  But while
it might be possible to get some inspiration by studying analogous compiler
work, my feeling is that a solution for a music language will have to exploit
music-specific attributes to simplify the problem.

<!--

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

TODO notes about block resize:

* Implement LRuler.inject.
  . This starts from the root block, and finds block callees.  If a callee
    occurs only once, and is 1:1 with the caller, copy the caller's ruler.
  . I can warn about blocks which aren't 1:1.
  . If it occurs >1, I could possibly put on multiple rulers, or I could
    just pick the first one.
  . I still need to retain start time so further modifications work.
    This is why I shouldn't store a separate start number, I should get it
    from the ranks.
* Handle [section, measure] generically.
* Move start_measure to Ruler so I don't have to parse MeterType.
* I don't like the extract/inject names.  Extract is too generic. Should
  be something like children to parent, parent to children.
* Insert/delete time that propagates up to callers.
  * Add the time in the bottom block.
  * When I move events, move the children too.
* When I add time, I need to also update the ruler.
  . This is tricky because I need to know where to delete or add ruler.
  . If there are multiple note tracks, then I can't know which one should
    get ruler changes, but it should only be one.  I could guess the first
    one, or the one with the most calls, but let's just require that there
    is a single one.
  . For this I need to keep track of the update offset within the event.
  . Implement update_ruler:
    . 'caller_updates' has to return a call tree, along with the caller
      positions.
    . 'apply_updates' uses that to move events.
    . 'update_rulers' uses it to determine that ranges at the top
      correspond to the affected (added or deleted) range at the bottom.
      This is why 1:1 is important!
    . Then I can splice or delete those top ranges.
    . To add event durations: block order doesn't matter, event order is
      highest to lowest.
    . To update the ruler: if I copy from the bottom to the top, then
      I need to go in child->parent order.  If I copy directly to the top
      and then use push_down, then block order doesn't matter.
    . push_down will also fix the measure numbers, which I'll want to do
      anyway.  push_down also handles the parent->child order.
    . What to do if affected events overlap?  If I'm deleting time, I just
      merge the ranges and delete that.  Otherwise it's complicated.
    . I have to add a set amount of time to the top block, which is the
      max of the the track deltas.
  . But I guess push_down can't work with overlapping events anyway.
    To get measure numbers right, it can't even deal with
    non-overlapping repeats.
    really what I want is to add some time.  I'm just adding a set
    amount of time at the bottom, and due to multiple events that may
    get multiplied.
  . What if I went the other way, added time at the top and pushed it
    down?
    . This way the amount of time never changes.  It propagates into all
      of the calls it overlaps, but will always have the same delta.
    . The ruler becomes just push_down, I think.
    . The problem is that it's more convenient to work at the bottom.  But
      if I can make the "highlight in callee" feature go the other way,
      maybe it's not so bad.
    . But then I have the same problem with repeated calls.  The time will
      of course get added to all of them, but I need to resize their
      events wherever they occur.
    . So I think this is no good, it has the same problem only worse
      because it doesn't give me a natural way to find those other
      callees.  Basically the "amount of time doesn't change" is not true.
  . So back to bottom-up.
    . Actually the change in block length is not  the max of the time on
      each track, because I still multiple the time if the events are
      non-overlapping.  It's because two overlapping events share their
      time delta.
    . No wait, that's not true.  It's applies only to two events in the
      same track, because shortening one is going to move back the other.
      Ok, so I can do max track delta.
    . Still, the ruler to add for overlapping events is ambiguous, because
      they put the callee at different parts of the meter.
    . Of course maybe all this messing with meter is getting ahead of
      myself.  Maybe I first implement just the event duration changes.
      Then I worry about setting meter... maybe I just want to double the
      meter at the top level and the callees are not meant to line up in
      a constant way.  Then, to serve the common case of adding a single
      measure I can come up with an extension for that.
  . It can't be done in general because the meter may be irregular, and
    you can't know what kind of meter to append.
  . I could have a thing that gets the meter in the selection, and adds
    another one of those at the selection.
  . A whole meter would be more useful to retain the structure.  I'll
    still need a delete range though, so I could add the measure and
    then delete the unwanted beats.
  . But the common infrastructure is to add/delete time, then find callers
    and apply recursively.
  . To extend rulers, I could splice in the bit of ruler from the callees.
  , Or I could just do a ruler pull_up on each affected block.  The latter
    would destroy any trailing silence though.
  . Or I could assume the toplevel ruler is right, and do a push_down...
    but it's probably wrong since I'm editing from the bottom.
  . I could require a -- event to explicitly mark the end, and otherwise
    assume that rulers for score blocks are managed automatically.
  . Splicing the ruler from the callee would also do the right thing by
    moving down non-meter ruler bits like cue marks.  Not that I use
    those.  Well, it might be the right thing.  Or the cue might be tied
    to time, not score.
  . Of course pull_up only works on a single track.  So this breaks down
    with multiple simultaneous callees.  In that case there is no single
    meter, so I have to say the top level is canonical, and push_down.
  . Wait, the problem with pull_up is how do I get start_measure correct?
    That's the whole reason I wrote push_down in the first place!  I think
    I have to find the roots, splice in the new bit of meter, then push
    down.  But knowing where to splice in the new meter means propagating
    the ranges from the bottom... I should have this available from
    'caller_updates', but I need it to retain parent-child relationships.
    I actually need that anyway, so I can know which blocks are the roots.
-->
