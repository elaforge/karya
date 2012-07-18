-- | Data for creating drum instruments.  It has few dependencies so it can
-- be imported by both Local.Instrument definitions and Derive.Call instrument
-- calls.
module Cmd.Instrument.Drums where
import qualified Data.List as List

import Util.Control
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import Derive.Attrs
import qualified Derive.Score as Score


-- | Description of a generic drum set.  There are many drum set instruments,
-- each of which probably use different MIDI keys, but at least I can
-- standardize call names, attributes, and keymap key.  Of course there will be
-- drum sets that don't fit in (e.g. have two or three snares), but at least
-- this provides a standard base.
data Note = Note {
    note_name :: String
    , note_attrs :: Attributes
    , note_char :: Char
    } deriving (Show)


c_bd    = Note "bd"     bd              'z'
c_sn    = Note "sn"     snare           'x'
c_ltom  = Note "ltom"   (tom <> low)    'c'
c_mtom  = Note "mtom"   (tom <> middle) 'v'
c_htom  = Note "htom"   (tom <> high)   'b'

c_hh    = Note "hh"     hh              'q'
c_ohh   = Note "ohh"    (open <> hh)    'q'
c_chh   = Note "chh"    (closed <> hh)  'w'
c_phh   = Note "phh"    (pedal <> hh)   'e'

c_ride  = Note "ride"   ride            't'
c_crash = Note "crash"  crash           'y'

-- TODO other drum style ornaments like double strikes, rolls, etc.

-- * kendang bali

kendang_composite :: [(Note, (Maybe Attributes, Maybe Attributes), Midi.Key)]
kendang_composite = map find_key
    [ (Note "PL" (wadon <> plak) 'b', (Just plak, Nothing))
    -- right
    , (Note "+" (wadon <> de)    'z', (Just de, Nothing))
    , (Note "o" (lanang <> de)   'x', (Nothing, Just de))
    , (Note "u" (wadon <> tut)   'c', (Just tut, Nothing))
    , (Note "U" (lanang <> tut)  'v', (Nothing, Just tut))
    -- left
    , (Note "k" (wadon <> pak)   'q', (Just pak, Nothing))
    , (Note "P" (lanang <> pak)  'w', (Nothing, Just pak))
    , (Note "t" (wadon <> pang)  'e', (Just pang, Nothing))
    , (Note "T" (lanang <> pang) 'r', (Nothing, Just pang))
    ]
    where
    -- The composite notes map to notes on a single kendang, so find them
    -- over there.
    find_key (note, attrs) = (note, attrs,
        key_of (note_attrs note `Score.attrs_diff` (wadon <> lanang)))
    key_of attrs = key
        where
        Just (_, key) = List.find ((==attrs) . note_attrs . fst)
            kendang_tunggal

kendang_tunggal :: [(Note, Midi.Key)]
kendang_tunggal =
    [ (Note "PL" plak            'b', Key.g1)
    -- right
    , (Note "+"  de              'z', Key.c2)
    , (Note "-"  (de <> soft)    'x', Key.c2)
    , (Note "+." (de <> thumb)   'c', Key.f2)
    , (Note "o"  tut             'v', Key.c3)
    , (Note "."  ka              'b', Key.g3)
    -- left
    , (Note "T"  pang            'q', Key.g4)
    , (Note "P"  pak             'w', Key.c5)
    , (Note "^"  (pak <> soft)   'e', Key.c5)
    , (Note "="  (de <> left)    'r', Key.d4) -- TODO d4 not set yet
    , (Note "`O+`" (tut <> left) 't', Key.c4)
    -- TODO cedugan
    ]
