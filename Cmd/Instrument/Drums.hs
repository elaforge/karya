-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Data for creating drum instruments.  It has few dependencies so it can
-- be imported by both Local.Instrument definitions and Derive.Call instrument
-- calls.
module Cmd.Instrument.Drums where
import Util.Control
import Derive.Attrs
import qualified Derive.TrackLang as TrackLang
import qualified Perform.Signal as Signal


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

c_bd    = Note "bd"     bd              'z' 1
c_bd2   = Note "bd2"    (bd <> v2)      's' 1
c_sn    = Note "sn"     snare           'x' 1
c_sn2   = Note "sn2"    (snare <> v2)   'd' 1
c_rim   = Note "rim"    rim             'v' 1
c_ltom  = Note "ltom"   (tom <> low)    'b' 1
c_mtom  = Note "mtom"   (tom <> middle) 'n' 1
c_htom  = Note "htom"   (tom <> high)   'm' 1

-- Also doubles as closed hh, if both exist.
c_hh    = Note "hh"     hh              'q' 1
c_ohh   = Note "ohh"    (open <> hh)    'w' 1
c_phh   = Note "phh"    (pedal <> hh)   'e' 1

c_ride  = Note "ride"   ride            't' 1
c_crash = Note "crash"  crash           'y' 1

-- TODO other drum style ornaments like double strikes, rolls, etc.
