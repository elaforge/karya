-- | Tags categorize calls, and can be used to organize or filter
-- documentation.
--
-- TODO many of these document internal properties of a call, and it would be
-- better to extract them automatically, e.g. 'next', 'inverting', 'subs', etc.
module Derive.Call.Tags where
import Util.Control
import qualified Data.Set as Set


type Tags = Set.Set Tag
newtype Tag = Tag String deriving (Show, Ord, Eq)

tag :: String -> Tags
tag = Set.singleton . Tag

untag :: Tags -> [String]
untag = map (\(Tag t) -> t) . Set.toList

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


-- * automatically applied

generator = tag "generator"
transformer = tag "transformer"

note = tag "note"
pitch = tag "pitch"
control = tag "control"
val = tag "val"
