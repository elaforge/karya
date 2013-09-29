[back to overview](overview.md.html#karya)

## GUI

<img align=right width=180 src="../../doc/img/ly-example.png">

The GUI only has one kind of window, and that is a view on a block, which is
the top level of score organization.  All blocks have the same physical
structure, which boils down to text in various places.  How that is interpreted
and becomes music is the business of the deriver.  So the UI elements described
here have links to a parallel structure in the
[derivation doc.](derivation.md.html)

One important thing to know about the UI is that it is, like the rest of the
program, organized in many layers.  The lowest layer is the C++ API, which just
opens windows, changes colors, etc. but has no idea what any of that means.
This is represented mostly by the API exposed in 'Ui.Block.BlockC', which is a
direct wrapper around the C++ API.  The medium level is mostly dumb, in that
you alter haskell data structures and the UI then changes to reflect the
changes, but you are still free to alter them in just about any way you see
fit.  This level is what you see if you directly manipulate 'Ui.State.State',
which you could do if you wanted, or slightly higher level if you use the
functions in 'Ui.State', which is what you should probably do.  The high level
is defined by what Cmds are available, and it is at this level that UI changes
have "meaning".  For example, a "K" in the edit box means that kbd entry is
enabled only because there is a Cmd that notices when kbd entry is enabled
('Cmd.Internal.sync_edit_box') and calls 'Ui.State.set_edit_box'.  This is all
to say that most of what is described here are simply the conventions
established by the default set of Cmds, and if you write your own Cmds or
override the defaults you can change things arbitrarily.  So you could put
other letters in the edit mode box if you wrote a Cmd to do that.

Each block is named by a [BlockId](#ids), and the name part of
the ID is in the window title, followed by the [ViewId](#ids).

Below that, there is a text input, which is the block title.  This actually
hides itself when it's empty, and you can get it to come back by double
clicking on the skeleton display.

The [skeleton display](derivation.md.html#derivation) has arrows from track to
track.

To the left of a the skeleton is a blank gray box that displays the [edit
mode](#editmode), and below that is the [ruler](#ruler).

To the right of the ruler are vertical [tracks](#tracks), which is the bulk of
the window.  The tracks have [events](#events), each of which has a red line at
its trigger point, a duration, and a bit of text.  Some events (for instance,
most events on control or pitch tracks) have a 0 duration, so they're just a
red trigger line and a bit of text.  Tracks and events are color coded based on
their type.  This doesn't have any semantic meaning, and is analogous to syntax
highlighting.  The tracks may also have any number of
[selections](#selections).

The bottom of the window has a status display, which displays some highly
abbreviated bits of app state.  The various status fields are from the "status
view" section of 'App.Config'.

There's another tiny little gray box in the lower left corner.  This shows play
status: gray when ready to play, light blue when the score needs to be
rederived, dark blue when derivation is in progress, and green when playing.

That's all!

### text input

Text input boxes expand temporarily if their text is too large to fit.  It's
not very satisfactory, but I couldn't come up with a better way to cram
text into small spaces.  Track titles still usually wind up being too short to
display their contents.

Control-h and control-l skip backward and forward by a token, where a token is
a space separated word, a `symbol`, or a parenthesized expression.  TODO maybe
it should just be a word.  Holding shift extends the selection as usual.
Control-backspace deletes a token.  Otherwise, they use the shortcuts
documented on fltk's Fl_Input.

## IDs

IDs are names for the major score elements.  ViewIds name 'Ui.Block.View's,
BlockIds name 'Ui.Block.Block's, RulerIds name 'Ui.Ruler.Ruler's, and TrackIds
name 'Ui.Track.Track's.  When you want to do something with one of those, say
create a view for a block, modify a ruler, or whatever, you wind up using the
ID to name it.  Ultimately IDs all turn into lookups on the maps in
'Ui.State.State'.

Each ID has a namespace and a name component, e.g.  `(bid "namespace/name")` is
a BlockId.  Each score has a default namespace
'Ui.StateConfig.config_namespace'.  New IDs get the default namespace, and when
writing a block call you normally just write the name, leaving the namespace
implicit.  The result is that you should be able to merge two scores together
and avoid ID clashes.

Identifier naming is more strict than most languages.  Only lowercase letters
a-z, digits 0-9, period and hyphen are allowed.  IDs also allow \`s so BlockIds
can embed symbols.  This enforces a lowercase-with-hyphens naming scheme.  The
exact definition is in 'Derive.ParseBs.p_identifier'.

The intent of the restrictive rules are that they relieve me of the burden of
remembering a naming scheme (e.g. '.' vs. '-' vs. '_') and leave room for
flexibility in other places, e.g. @-macros in the [REPL](repl.md.html).

## Cmds

All the interesting things you can do with 'Ui.State.State' is facilitated by
cmds, so you should look at the [cmd documentation](cmd.md.html).
