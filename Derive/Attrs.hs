-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | A common library of attributes for instrument definitians.

    Usually attributes act like tags in that they all get combined into one
    'Attributes' set.  This makes sense in some cases, but not all.  For
    example, @+mute@ and @+cresc@ can be usefully combined, you could imagine
    applying @+mute@ to the entire melody and wanting that to combine with
    individual articulations within.  But @+pizz-right@ is an articulation on
    its own, and it doesn't really make sense to, e.g. put @+right@ on melody
    by itself.

    And of course since attributes are all mashed into one set, the information
    about which ones were applied together is lost.  For example, I think it's
    impossible for the lilypond backend to figure how to notate @+pizz+right@
    inside of @+pizz@ since right-hand pizz is applied as a per-note
    articulation, while plain pizz is a mode.  But it's trivial if the
    attribute is @+pizz-right@ instead.
-}
module Derive.Attrs where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Serialize as Serialize
import qualified Derive.ShowVal as ShowVal
import Global


-- | Instruments can have a set of attributes along with them.  These are
-- propagated dynamically down the derivation stack.  They function like
-- arguments to an instrument, and will typically select an articulation, or
-- a drum from a drumset, or something like that.
type Attribute = Text

newtype Attributes = Attributes (Set Attribute)
    deriving (Semigroup, Monoid, Eq, Ord, Read, Show, Serialize.Serialize,
        DeepSeq.NFData, Aeson.ToJSON, Aeson.FromJSON)

instance Pretty Attributes where pretty = ShowVal.show_val
instance ShowVal.ShowVal Attributes where
    show_val = ("+"<>) . Text.intercalate "+" . to_list

attr :: Text -> Attributes
attr = Attributes . Set.singleton

attrs :: [Text] -> Attributes
attrs = Attributes . Set.fromList

from_set :: Set Attribute -> Attributes
from_set = Attributes

to_set :: Attributes -> Set Attribute
to_set (Attributes attrs) = attrs

to_list :: Attributes -> [Attribute]
to_list = Set.toList . to_set

difference :: Attributes -> Attributes -> Attributes
difference (Attributes x) (Attributes y) = Attributes (Set.difference x y)

intersection :: Attributes -> Attributes -> Attributes
intersection (Attributes x) (Attributes y) = Attributes (Set.intersection x y)

-- | True if the first argument contains the attributes in the second.
contain :: Attributes -> Attributes -> Bool
contain (Attributes super) (Attributes sub) = sub `Set.isSubsetOf` super

-- | Remove the attributes in the first argument from the second.
remove :: Attributes -> Attributes -> Attributes
remove (Attributes remove) (Attributes attrs) =
    Attributes $ attrs `Set.difference` remove


-- * articulations

flaut = attr "flaut" -- flautando
staccato = attr "stac"
spiccato = attr "spic"
detache = attr "det"
marcato = attr "marc"
portato = attr "port"
tenuto = attr "tenuto"
accent = attr "accent"
espr = attr "espr" -- espressivo
trill = attr "trill"
trem = attr "trem" -- tremolo
arpeggio = attr "arp"
legato = attr "legato"
porta = attr "porta" -- portamento
gliss = attr "gliss" -- glissando
mute = attr "mute" -- generic muted note

vib = attr "vib" -- vibrato
nv = attr "nv" -- no vibrato

-- * dynamics

cresc = attr "cresc"
dim = attr "dim" -- either diminuendo, or diminished
sfz = attr "sfz"
sffz = attr "sffz"
fp = attr "fp"
pfp = attr "pfp"
fpf = attr "fpf"
soft = attr "soft"
loud = attr "loud"

-- Whole note and half note variants, e.g. for trills.
half = attr "half"
whole = attr "whole"

-- * strings

arco = attr "arco"
pizz = attr "pizz" -- pizzicato
pizz_right = attr "pizz-right"
snap = attr "snap" -- snap pizz, aka Bartok pizz
harm = attr "harm"
legno = attr "legno"
sord = attr "sord" -- con sordino

-- Artificial or natural, presumably for harmonics.
natural = attr "nat"
artificial = attr "art"

pont = attr "pont" -- ponticello
tasto = attr "tasto" -- sul tasto

bisbig = attr "bisbig" -- bisbigliando

-- * winds

flutter = attr "flutter"

-- * drums

snare = attr "snare" -- snare
stick = attr "stick"
bd = attr "bd" -- base drum
hh = attr "hh" -- high-hat
crash = attr "crash" -- crash cymbal
ride = attr "ride" -- ride cymbal
tom = attr "tom"

pedal = attr "pedal"

-- ** technique

left = attr "left" -- played with left hand
right = attr "right" -- played with right hand
thumb = attr "thumb"

center = attr "center"
edge = attr "edge"
slap = attr "slap"
knuckle = attr "knuckle"
rim = attr "rim"
damp = attr "damp"

open = attr "open"
closed = attr "closed"

low = attr "low"
middle = attr "middle"
high = attr "high"

up = attr "up"
down = attr "down"

short = attr "short"
long = attr "long"

fast = attr "fast"
medium = attr "medium"
slow = attr "slow"

light = attr "light"
heavy = attr "heavy"

loose = attr "loose"
tight = attr "tight"

dry = attr "dry"
wet = attr "wet"

-- * misc

-- Many of these attributes are meant to be combined with others.

-- | Generic variations.  E.g. if there are two snares you can have
-- @snare \<> v1@ and @snare \<> v2@.
v1 = attr "v1"
v2 = attr "v2"
v3 = attr "v3"
v4 = attr "v4"
v5 = attr "v5"
v6 = attr "v6"

-- | Ordinal numbers, either for intervals, or for the nth of something (e.g.
-- harm <> third).
first = attr "first"
second = attr "second"
third = attr "third"
fourth = attr "fourth"
fifth = attr "fifth"
sixth = attr "sixth"
seventh = attr "seventh"
