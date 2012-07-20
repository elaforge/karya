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
import qualified Perform.Signal as Signal


-- | Description of a generic drum set.  There are many drum set instruments,
-- each of which probably use different MIDI keys, but at least I can
-- standardize call names, attributes, and keymap key.  Of course there will be
-- drum sets that don't fit in (e.g. have two or three snares), but at least
-- this provides a standard base.
data Note = Note {
    note_name :: String
    , note_attrs :: Attributes
    , note_char :: Char
    -- | Scale the dynamic by this value.  This is for drums that have
    -- different symbols for soft strokes.
    , note_dynamic :: Signal.Y
    } deriving (Show)


c_bd    = Note "bd"     bd              'z' 1
c_sn    = Note "sn"     snare           'x' 1
c_ltom  = Note "ltom"   (tom <> low)    'c' 1
c_mtom  = Note "mtom"   (tom <> middle) 'v' 1
c_htom  = Note "htom"   (tom <> high)   'b' 1

c_hh    = Note "hh"     hh              'q' 1
c_ohh   = Note "ohh"    (open <> hh)    'q' 1
c_chh   = Note "chh"    (closed <> hh)  'w' 1
c_phh   = Note "phh"    (pedal <> hh)   'e' 1

c_ride  = Note "ride"   ride            't' 1
c_crash = Note "crash"  crash           'y' 1

-- TODO other drum style ornaments like double strikes, rolls, etc.

-- * kendang bali

kendang_composite :: [(Note, (Maybe Attributes, Maybe Attributes), Midi.Key)]
kendang_composite = map find_key
    [ (Note "PL" (wadon <> plak) 'b' 1, (Just plak, Nothing))
    , (Note "`O+`" (lanang <> tut <> left) 't' 1, (Nothing, Just (tut <> left)))
    -- right
    , (Note "+" (wadon <> de)    'z' 1, (Just de, Nothing)) -- de
    , (Note "o" (lanang <> de)   'x' 1, (Nothing, Just de)) -- tut
    , (Note "u" (wadon <> tut)   'c' 1, (Just tut, Nothing)) -- kum
    , (Note "U" (lanang <> tut)  'v' 1, (Nothing, Just tut)) -- pung
    -- left
    , (Note "k" (wadon <> pak)   'q' 1, (Just pak, Nothing)) -- ka
    , (Note "P" (lanang <> pak)  'w' 1, (Nothing, Just pak)) -- pak
    , (Note "t" (wadon <> pang)  'e' 1, (Just pang, Nothing)) -- kam
    , (Note "T" (lanang <> pang) 'r' 1, (Nothing, Just pang)) -- pang
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
    [ (Note "PL" plak            'b' 1.0, Key.g1)
    -- right
    , (Note "+"  de              'z' 1.0, Key.c2)
    , (Note "-"  (de <> soft)    'x' 0.3, Key.c2)
    , (Note "+." (de <> thumb)   'c' 1.0, Key.f2)
    , (Note "o"  tut             'v' 1.0, Key.c3)
    , (Note "."  (ka <> soft)    'b' 0.3, Key.g3)
    -- This should be rarely used, but '.' should definitely be soft, but
    -- if it is there is no way to emit a normal 'ka'.
    , (Note "_"  ka              'n' 1.0, Key.g3)
    -- left
    , (Note "T"  pang            'q' 1.0, Key.g4)
    , (Note "P"  pak             'w' 1.0, Key.c5)
    , (Note "^"  (pak <> soft)   'e' 0.3, Key.c5)
    , (Note "="  (de <> left)    'r' 1.0, Key.d4) -- TODO d4 not set yet
    , (Note "`O+`" (tut <> left) 't' 1.0, Key.c4)
    -- TODO cedugan
    ]
