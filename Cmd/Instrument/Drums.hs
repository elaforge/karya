-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Data for creating drum instruments.  It has few dependencies so it can
-- be imported by both Local.Instrument definitions and Derive.Call instrument
-- calls.
module Cmd.Instrument.Drums where
import qualified Util.Pretty as Pretty
import Derive.Attrs
import qualified Derive.TrackLang as TrackLang
import qualified Perform.Signal as Signal
import Global


-- | Description of a generic drum set.  There are many drum set instruments,
-- each of which probably use different MIDI keys, but at least I can
-- standardize call names, attributes, and keymap key.  Of course there will be
-- drum sets that don't fit in (e.g. have two or three snares), but at least
-- this provides a standard base.
data Note = Note {
    note_name :: !TrackLang.CallId
    , note_attrs :: !Attributes
    , note_char :: !Char
    -- | Scale the dynamic by this value.  This is for drums that have
    -- different symbols for soft strokes.
    , note_dynamic :: !Signal.Y
    } deriving (Show)

note :: TrackLang.CallId -> Attributes -> Char -> Note
note name attrs char = Note name attrs char 1

instance Pretty.Pretty Note where
    format (Note name attrs char dyn) = Pretty.record "Note"
        [ ("name", Pretty.format name)
        , ("attrs", Pretty.format attrs)
        , ("char", Pretty.format char)
        , ("dynamic", Pretty.format dyn)
        ]

c_bd    = note "bd"     bd              'z'
c_bd2   = note "bd2"    (bd <> v2)      's'
c_sn    = note "sn"     snare           'x'
c_sn2   = note "sn2"    (snare <> v2)   'd'
c_rim   = note "rim"    rim             'v'
c_ltom  = note "ltom"   (tom <> low)    'b'
c_mtom  = note "mtom"   (tom <> middle) 'n'
c_htom  = note "htom"   (tom <> high)   'm'

-- Also doubles as closed hh, if both exist.
c_hh    = note "hh"     hh              'q'
c_ohh   = note "ohh"    (open <> hh)    'w'
c_phh   = note "phh"    (pedal <> hh)   'e'

c_ride  = note "ride"   ride            't'
c_crash = note "crash"  crash           'y'

-- TODO other drum style ornaments like double strikes, rolls, etc.
