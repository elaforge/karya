-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Data for creating drum instruments.  It has few dependencies so it can
-- be imported by both Local.Instrument definitions and Derive.Call instrument
-- calls.
module Cmd.Instrument.Drums where
import qualified Util.Pretty as Pretty
import qualified Derive.Expr as Expr
import qualified Perform.Signal as Signal

import           Derive.Attrs
import           Global


-- | Description of a generic drum set.  There are many drum set instruments,
-- each of which probably use different MIDI keys, but at least I can
-- standardize call names, attributes, and keymap key.  Of course there will be
-- drum sets that don't fit in (e.g. have two or three snares), but at least
-- this provides a standard base.
data Stroke = Stroke {
    _name :: !Expr.Symbol
    , _attributes :: !Attributes
    , _char :: !Char
    -- | Scale the dynamic by this value.  This is for drums that have
    -- different symbols for soft strokes.
    , _dynamic :: !Signal.Y
    , _group :: !Group
    } deriving (Eq, Show)

-- | An arbitrary symbol.  A group can stop other groups from sounding.
type Group = Text

-- | Pair each stopping group with the list of groups it stops.
type Stops = [(Group, [Group])]

stroke :: Char -> Expr.Symbol -> Attributes -> Stroke
stroke char name attrs = Stroke
    { _name = name
    , _attributes = attrs
    , _char = char
    , _dynamic = 1
    , _group = ""
    }

stroke_dyn :: Char -> Expr.Symbol -> Attributes -> Signal.Y -> Stroke
stroke_dyn char name attrs dyn = (stroke char name attrs) { _dynamic = dyn }

instance Pretty Stroke where
    format (Stroke name attrs char dyn group) = Pretty.record "Stroke"
        [ ("name", Pretty.format name)
        , ("attrs", Pretty.format attrs)
        , ("char", Pretty.format char)
        , ("dynamic", Pretty.format dyn)
        , ("group", Pretty.format group)
        ]

c_bd    = stroke 'z' "bd"     bd
c_bd2   = stroke 's' "bd2"    (bd <> v2)
c_sn    = stroke 'x' "sn"     snare
c_sn2   = stroke 'd' "sn2"    (snare <> v2)
c_rim   = stroke 'v' "rim"    rim
c_ltom  = stroke 'b' "ltom"   (tom <> low)
c_mtom  = stroke 'n' "mtom"   (tom <> middle)
c_htom  = stroke 'm' "htom"   (tom <> high)

-- Also doubles as closed hh, if both exist.
c_hh    = stroke 'q' "hh"     hh
c_ohh   = stroke 'w' "ohh"    (open <> hh)
c_phh   = stroke 'e' "phh"    (pedal <> hh)

c_ride  = stroke 't' "ride"   ride
c_crash = stroke 'y' "crash"  crash

-- TODO other drum style ornaments like double strikes, rolls, etc.
