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
under_invert :: Tags
under_invert = tag "under-invert"

-- * normal

-- | Internal calls are used to implement the basic track calls.  You should
-- never need to call them directly, and they can probably be omitted from the
-- documentation.
internal :: Tags
internal = tag "internal"

-- | This marks a standard library of \"fundamental\" calls.  They may also
-- interact more intimately with the builtin derivation machinery.
prelude :: Tags
prelude = tag "prelude"

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

-- | Val call is a scale degree.
scale :: Tags
scale = tag "scale"

-- | Call uses randomness, and depends on the random seed.
random :: Tags
random = tag "random"

-- | Adds note doohickeys to an existing note.
ornament :: Tags
ornament = tag "ornament"

-- | Depends on the previous event.
prev :: Tags
prev = tag "prev"

-- | Depends on the next event.
next :: Tags
next = tag "next"

-- | Adds or transforms note in a style idiomatic to a particular instrument.
idiom :: Tags
idiom = tag "idiom"

-- | Transforms Score.Events.
postproc :: Tags
postproc = tag "postproc"

-- | Adds (or removes) attributes from notes.
attr :: Tags
attr = tag "attr"

-- | Can emit lilypond.
ly :: Tags
ly = tag "ly"

-- | Only emits lilypond, emits no \"normal\" events.
ly_only :: Tags
ly_only = ly <> tag "ly-only"

-- * idioms

bali :: Tags
bali = tag "bali"

india :: Tags
india = tag "india"


-- * automatically applied

generator = tag "generator"
transformer = tag "transformer"

note = tag "note"
pitch = tag "pitch"
control = tag "control"
val = tag "val"
