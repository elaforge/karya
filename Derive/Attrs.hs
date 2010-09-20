-- | A common library of attributes for instrument definitians.
--
-- This is meant to be imported unqualified.
module Derive.Attrs where
import qualified Data.Monoid as Monoid
import qualified Derive.Score as Score
import Derive.Score (Attributes, attr)


(@+) :: Attributes -> Attributes -> Attributes
(@+) = Monoid.mappend

no_attrs :: Attributes
no_attrs = Score.no_attrs

-- * articulations

cresc = attr "cresc"
dim = attr "dim"
sfz = attr "sfz"

-- * strings

pizz = attr "pizz" -- pizzicato
trem = attr "trem" -- tremolo

-- * drums

-- ** balinese

lanang = attr "lanang"
wadon = attr "wadon"
kebyar = attr "kebyar"
legong = attr "legong"

-- ** western

snare = attr "snare" -- snare
bd = attr "bd" -- base drum
hh = attr "hh" -- high-hat
crash = attr "crash" -- crash cymbal
ride = attr "ride" -- ride cymbal
tom = attr "tom"

-- * technique

left = attr "left" -- played with left hand
right = attr "right" -- played with right hand

center = attr "center"
edge = attr "edge"
slap = attr "slap"
knuckle = attr "knuckle"

open = attr "open"
closed = attr "closed"

low = attr "low"
middle = attr "middle"
high = attr "high"

-- * misc

-- Many of these attributes are meant to be combined with others.

-- | Generic variations.  E.g. if there are two snares you can have
-- @snare \@+ v1@ and @snare \@+ v2\@.
v1 = attr "1"
v2 = attr "2"
v3 = attr "3"
v4 = attr "4"
