-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Tags categorize calls, and can be used to organize or filter
-- documentation.
--
-- TODO many of these document internal properties of a call, and it would be
-- better to extract them automatically, e.g. 'next', 'inverting', 'subs', etc.
module Derive.Call.Tags where
import Util.Control
import qualified Data.Set as Set


type Tags = Set.Set Tag
newtype Tag = Tag Text deriving (Show, Ord, Eq)

tag :: Text -> Tags
tag = Set.singleton . Tag

untag :: Tags -> [Text]
untag = map (\(Tag t) -> t) . Set.toList

contains :: Tags -> Tags -> Bool
contains super sub = Set.isSubsetOf sub super

-- * special

-- | This is a siganl that this transformer wants to run under inversion.
--
-- Normally when a call is inverted, the transformers run outside the
-- inversion, while only the generator runs underneath.  However, some
-- transformers rely on per-note controls, such as pitch and dyn, and therefore
-- need to go under the invert.  A special hack in
-- 'Derive.Call.apply_transformers' notices the presence of this tag, and
-- delays a transformer to run under inversion if present.
--
-- See NOTE [under_invert].
under_invert :: Tags
under_invert = tag "under-invert"

-- * normal

-- | Takes sub-events.
subs :: Tags
subs = tag "subs"

-- | Inverting call.
inverting :: Tags
inverting = tag "inverting"

-- | Uses control modification, this is a control or pitch call that can modify
-- controls other than the control track in appears in.
cmod :: Tags
cmod = tag "cmod"

-- | This call returns a 'TrackLang.VControlFunction'.
control_function :: Tags
control_function = tag "control-function"

-- | Call uses randomness, and depends on the random seed.
random :: Tags
random = tag "random"

-- | Adds ornaments an existing note, generally to its attack.
ornament :: Tags
ornament = tag "ornament"

-- | Depends on the previous event.
prev :: Tags
prev = tag "prev"

-- | Depends on the next event.
next :: Tags
next = tag "next"

-- | This call emits events with the 'Derive.Environ.args' set, and requires
-- a 'postproc' call to emit the proper events.
requires_postproc :: Tags
requires_postproc = tag "requires-postproc"

-- | Adds or transforms note in a style idiomatic to a particular instrument.
inst :: Tags
inst = tag "inst"

-- | Transforms Score.Events.
postproc :: Tags
postproc = tag "postproc"

-- | Adds (or removes) attributes from notes.
attr :: Tags
attr = tag "attr"

-- | Can emit lilypond.
ly :: Tags
ly = tag "ly"

-- * genres


-- * automatically applied

generator = tag "generator"
transformer = tag "transformer"

note = tag "note"
pitch = tag "pitch"
control = tag "control"
val = tag "val"

{- NOTE [under_invert]
    . To make lift to an absolute pitch work outside of inversion, I'd need
      an abstract way (e.g. like a transpose signal) to say "pitch midway to
      (4c)"
    . It's better to have the lift under the pitch.  The only reason it isn't
      is that inversion assumes all transformers go above.  So either make it
      a generator (at which point it can't compose), or have some way to put
      transformers under the inversion, e.g. 'delay | Drop $ lift $ gen' under
      inversion is 'delay' -> 'Drop' 'lift' 'gen'.
    . Another way would be to put that in the call itself, so 'lift' has a flag
      that says it likes to be under the inversion.  Then the invert function
      has to go look all those up.  But that can't work, because invert is
      called by a generator, and that's too late.
    . So call all the transformers pre and post invert.  Normally they check
      if they're under inversion, and if so do nothing, but ones that would
      rather be inverted do the inverse.

    Cons:
      1. Instead of transformers always happening before inversion, they can
      now vary internally, which is one more subtle thing about inversion.
      I'll need to expose it in documentation at least, via a tag.

      2. Call stacks get even messier in the presence of inversion, since
      every transformer appears twice.

      3. Transformers can have their order change, e.g. given
      'below | above | gen', below is actually called below above, if it
      wants to be under inversion.

    . It seems like I could improve these by driving them from a tag.  E.g.
      if the call has a under-inversion tag, Call.apply_transformers will skip
      or not skip, as appropriate.  This solves #1 and #2, but not #3.

    . This is all just to get lift working under inversion.  Is it that
      important?
      . Everything should work under inversion.  It's a hassle to suddenly
        have to rearrange the pitch track, and now 'd' doesn't work.
      . This will come up for every note transformer that wants to know the
        pitch.
-}
