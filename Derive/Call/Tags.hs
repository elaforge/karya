-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Tags categorize calls, and can be used to organize or filter
-- documentation.
--
-- TODO many of these document internal properties of a call, and it would be
-- better to extract them automatically, e.g. 'next', 'inverting', 'subs', etc.
module Derive.Call.Tags where
import qualified Data.Set as Set

import Global


type Tags = Set.Set Tag
newtype Tag = Tag Text deriving (Show, Ord, Eq)

tag :: Text -> Tags
tag = Set.singleton . Tag

untag :: Tags -> [Text]
untag = map (\(Tag t) -> t) . Set.toList

contains :: Tags -> Tags -> Bool
contains super sub = Set.isSubsetOf sub super

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

-- | This transformer runs under inversion, via 'Derive.Call.Sub.under_invert'.
under_invert :: Tags
under_invert = tag "under-invert"

-- * automatically applied

generator, transformer :: Tags
generator = tag "generator"
transformer = tag "transformer"

note, pitch, control, val :: Tags
note = tag "note"
pitch = tag "pitch"
control = tag "control"
val = tag "val"
