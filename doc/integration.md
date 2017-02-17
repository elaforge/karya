## Integration

The key to having a high level score is to factor out repeated aspects.
There turn out to be a lot of different ways things are repeated in music, and
most of the complexity in Karya is concerned with different ways to factor out
repetition.

Calls are one approach.  A single ornament or idiomatic phrase can be factored
into a call, and parameterized with arguments and signals.  Similarly, a
section and a whole score is factored into a block call, and can still be
varied by means of different signals, tempo, etc.  If factoring fails, there's
always copy and paste, and since music often has repeats with small
idiosyncratic differences, factoring fails often.

Integration is a middle ground.  It takes the output of derivation and
integrates it back into the score.  It also keeps track of the source, and
when that changes will reintegrate back into the destination.  There is a
simplistic merge algorithm described in 'Cmd.Integrate.Merge'.  Each event
keeps track of its source, and can detect position, duration, and text changes,
but it's probably not terribly difficult to confuse it.

Since track structure is lost during derivation, integration has to recreate
it.  It's described by 'Cmd.Integrate.Convert.TrackKey', but basically it
tries to keep the source tracks and their order, while splitting overlapping
events and differing instruments.  It also splits on the 'Derive.EnvKey.voice'
variable, and calls that expect their output will be integrated can set this
to explicitly divide their output events into separate tracks.

As a visual reminder, events that were integrated from a source have their text
in italics, and it will be bold if the event hasn't been changed.

### block derive integration

Block integration is triggered by adding a `<<` as a note transformer.
It only works if you add it to a block title, because
'Ui.Block.block_integrated' keeps track of block integration by BlockId.

This will automatically create a new block with its contents as seen by the
transformer.  You can create another block manually with
'Cmd.Repl.LIntegrate.block'.

As a visual reminder, the status bar of an integrated block will have a red
background.

### track derive integration

Track integration allows one track to act as the source for other tracks on the
same block.  It's basically the same story as block integration, except you add
the `<` transformer to a note track title, and it will create a set of
integrated tracks on the same block.  Unlike block integrate, the `<`
transformer will not pass the events through, so the integrated track itself
won't sound when you play it.

Similar to block integration, adding the track integrate transformer
automatically creates new tracks if there were none, and
'Cmd.Repl.LIntegrate.track' will create additional tracks.

The integrated tracks are linked back to their source with red arrows.

### block and track score integration

This is a different variant of integration, that copies the UI events directly,
with no intervening derivation.  So it's not really "integration" in that it
doesn't have to try to reverse a derivation.  This means that all notation
will be preserved, and is useful if you simply want a variant copy of an
original block or track, rather than a lower level view of it.

Like derive integration, score integration has both block and track variants.
Unlike derive integration, there's no corresponding call, you directly add
the "integrated from" annotation to the relevant block or track.
'Cmd.Repl.LIntegrate.score_block' and 'Cmd.Repl.LIntegrate.score_track' do this
for the currently selected block or track.  Similar to derive integration,
score integration dependency is indicated with a greenish status line for a
block, or a green arrow for a track.
