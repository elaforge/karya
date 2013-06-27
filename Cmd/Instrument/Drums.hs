-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
    note_name :: !Text
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

-- * kendang bali

-- | Left is wadon, Right is lanang.
type CompositeAttrs = Either Score.Attributes Score.Attributes

data Kendang = Wadon | Lanang deriving (Show, Eq)
data Lima = Kebot | Kenawan deriving (Show)

kendang_composite :: [((Note, Midi.Key), (Attributes, Kendang))]
kendang_composite = map resolve
    [ ("PL", Wadon, plak, 'b', 1)
    , ("Ø", Lanang, tut <> left, 't', 1)
    -- kenawan
    , ("+", Wadon, de, 'z', 1) -- de
    , ("-", Wadon, de <> soft, 'x', 0.3) -- de
    , ("o", Lanang, de, 'c', 1) -- tut
    , ("u", Wadon, tut, 'v', 1) -- kum
    , ("U", Lanang, tut, 'b', 1) -- pung
    -- kebot
    , ("k", Wadon, pak, 'q', 1) -- ka
    , ("P", Lanang, pak, 'w', 1) -- pak
    , ("t", Wadon, pang, 'e', 1) -- kam
    , ("T", Lanang, pang, 'r', 1) -- pang
    ]
    where
    resolve (name, kendang, attrs, char, dyn) =
        ((Note name (kattr kendang <> attrs) char dyn, key_of attrs),
            (attrs, kendang))
    kattr Wadon = wadon
    kattr Lanang = lanang
    key_of attrs = key
        where
        Just (_, key) = List.find ((==attrs) . note_attrs . fst)
            (map fst kendang_tunggal)

kendang_tunggal :: [((Note, Midi.Key), Lima)]
kendang_tunggal =
    map (flip (,) Kenawan)
    [ (Note "PL" plak            'b' 1.0, Key.g1)
    , (Note "+"  de              'z' 1.0, Key.c2)
    , (Note "-"  (de <> soft)    'x' 0.3, Key.c2)
    , (Note "+." (de <> thumb)   'c' 1.0, Key.f2)
    , (Note "o"  tut             'v' 1.0, Key.c3)
    , (Note "."  (ka <> soft)    'b' 0.3, Key.g3)
    -- This should be rarely used, but '.' should definitely be soft, but
    -- if it is there is no way to emit a normal 'ka'.
    , (Note ".."  ka             'n' 1.0, Key.g3)
    , (Note "+/" (de <> mute)    'm' 1.0, Key.c1)
    ] ++ map (flip (,) Kebot)
    [ (Note "T"  pang            'q' 1.0, Key.g4)
    , (Note "P"  pak             'w' 1.0, Key.c5)
    , (Note "^"  (pak <> soft)   'e' 0.3, Key.c5)
    , (Note "`O+`" (de <> left)  'r' 1.0, Key.d4)
    , (Note "Ø"  (tut <> left)   't' 1.0, Key.c4)
    -- TODO cedugan
    ]
