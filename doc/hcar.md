report by: Evan Laforge
status: stable, active

Karya is a score writer / music sequencer.  It doesn't use 5-line staff
notation, but has its own semi-textual semi-graphical format.  It has
a library of scales, ornaments, instrumental idioms, and is extensible in
Haskell or its own language.  It has backends for MIDI, lilypond, and a
offline synthesis system.

In addition to the graphical score, it incorporates a Haskell-based DSL for
Carnatic percussion, along with a library of korvais, mohras, tirmanams,
and exercises.

Current work is on the offline synthesis backend, which includes a FAUST
wrapper and a sampler.  I'm extending them both to evaluate incrementally
so they're practical for real use.  Once incremental evaluation is working
well, I plan to explore physical modelling synthesis and other approaches
historically considered "too expensive."

Documentation: http://ofb.net/~elaforge/karya/doc/overview.md.html
Source: https://github.com/elaforge/karya
