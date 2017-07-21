-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Data for creating drum instruments.  It has few dependencies so it can
-- be imported by both Local.Instrument definitions and Derive.Call instrument
-- calls.
module Cmd.Instrument.Drums where
import qualified Util.Pretty as Pretty
import Derive.Attrs
import qualified Derive.Expr as Expr
import qualified Perform.Signal as Signal
import Global


-- | Description of a generic drum set.  There are many drum set instruments,
-- each of which probably use different MIDI keys, but at least I can
-- standardize call names, attributes, and keymap key.  Of course there will be
-- drum sets that don't fit in (e.g. have two or three snares), but at least
-- this provides a standard base.
data Note = Note {
    note_name :: !Expr.Symbol
    , note_attrs :: !Attributes
    , note_char :: !Char
    -- | Scale the dynamic by this value.  This is for drums that have
    -- different symbols for soft strokes.
    , note_dynamic :: !Signal.Y
    , note_group :: !Group
    } deriving (Eq, Show)

-- | An arbitrary symbol.  A group can stop other groups from sounding.
type Group = Text

note :: Char -> Expr.Symbol -> Attributes -> Note
note char name attrs = Note
    { note_name = name
    , note_attrs = attrs
    , note_char = char
    , note_dynamic = 1
    , note_group = ""
    }

note_dyn :: Char -> Expr.Symbol -> Attributes -> Signal.Y -> Note
note_dyn char name attrs dyn = (note char name attrs) { note_dynamic = dyn }

instance Pretty Note where
    format (Note name attrs char dyn group) = Pretty.record "Note"
        [ ("name", Pretty.format name)
        , ("attrs", Pretty.format attrs)
        , ("char", Pretty.format char)
        , ("dynamic", Pretty.format dyn)
        , ("group", Pretty.format group)
        ]

c_bd    = note 'z' "bd"     bd
c_bd2   = note 's' "bd2"    (bd <> v2)
c_sn    = note 'x' "sn"     snare
c_sn2   = note 'd' "sn2"    (snare <> v2)
c_rim   = note 'v' "rim"    rim
c_ltom  = note 'b' "ltom"   (tom <> low)
c_mtom  = note 'n' "mtom"   (tom <> middle)
c_htom  = note 'm' "htom"   (tom <> high)

-- Also doubles as closed hh, if both exist.
c_hh    = note 'q' "hh"     hh
c_ohh   = note 'w' "ohh"    (open <> hh)
c_phh   = note 'e' "phh"    (pedal <> hh)

c_ride  = note 't' "ride"   ride
c_crash = note 'y' "crash"  crash

-- TODO other drum style ornaments like double strikes, rolls, etc.
