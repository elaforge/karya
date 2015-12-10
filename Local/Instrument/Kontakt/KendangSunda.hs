-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Kendang sunda patches for "Local.Instrument.Kontakt".
module Local.Instrument.Kontakt.KendangSunda (patches, write_ksp) where
import qualified Data.Map as Map

import qualified Midi.Key2 as Key2
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Score as Score

import qualified Perform.NN as NN
import qualified Local.Instrument.Kontakt.Util as Util
import Global


patches :: [MidiInst.Patch]
patches =
    [ (CUtil.pitched_drum_patch pitched_notes $ patch "kendang-sunda", code)
    ]
    where
    patch name = MidiInst.patch (-24, 24) name []
    code = CUtil.drum_code (Just "kendang-tune") (map fst pitched_notes)

pitched_notes :: CUtil.PitchedNotes
(pitched_notes, _unmapped_notes) = CUtil.resolve_strokes 0.3 keymap strokes

strokes :: [(Char, BaseTypes.CallId, Score.Attributes, Drums.Group)]
stops :: [(Drums.Group, [Drums.Group])]
(stops, strokes) = (,) stops
    -- bang = dong + pak, plang = tong + pak, blang = dong + peung
    -- plak = left-closed + phak
    --
    -- with panggul
    -- indung, kiri
    [ ('a', ".",    dong <> soft,   left_open)
    -- should have argument version for variable pitch
    , ('z', "o",    dong,           left_open)
    , ('s', "o/",   dong <> Attrs.up, left_open)
    , ('x', "o^",   det,            left_open)
    , ('c', "i",    ting,           right_open)
    -- like 'o', should have argument version
    , ('v', "+",    tak,            left_closed)
    -- kulanter gede
    , ('v', "u",    tung,           kulanter_gede_open)
    -- indung, kanan
    , ('1', "^",    phak <> soft,   right_closed)
    , ('q', "P",    phak,           right_closed)
    , ('w', "I",    ping,           right_open)
    , ('e', "T",    pong,           right_open)
    -- kulanter leutik
    , ('r', "K",    pak,            kulanter_leutik_closed)
    , ('t', "E",    peung,          kulanter_leutik_open)
    ]
    where
    soft = Attrs.soft
    stops =
        [ (left_closed, [left_open])
        , (right_closed, [right_open])
        , (kulanter_leutik_closed, [kulanter_leutik_open])
        ]
    left_open = "left-open"
    left_closed = "left-closed"
    right_open = "right-open"
    right_closed = "right-closed"
    kulanter_gede_open = "kulanter-gede-open"
    kulanter_leutik_closed = "kulanter-leutik-closed"
    kulanter_leutik_open = "kulanter-leutik-open"

keymap :: Map.Map Score.Attributes CUtil.KeyswitchRange
keymap = CUtil.make_keymap Key2.e_2 Key2.c_1 12 NN.fs3
    []

write_ksp :: IO ()
write_ksp = mapM_ (uncurry Util.write)
    [ ("kendang-sunda.ksp",
        Util.drum_mute_ksp "kendang sunda" pitched_notes stops)
    ]

-- indung, kiri

-- | Open left hand stroke, low pitch.
dong :: Score.Attributes
dong = Score.attr "dong"

-- | Open left hand stroke, high pitch.
det :: Score.Attributes
det = Score.attr "det"

-- | Right side harmonic played on the left hand.
ting :: Score.Attributes
ting = Score.attr "ting"

-- | Closed left hand stroke.  This isn't an official name.
tak :: Score.Attributes
tak = Score.attr "tak"

-- indung, kanan

-- | Open right hand stroke.
ping :: Score.Attributes
ping = Score.attr "ping"

-- | Open right hand rim.
pong :: Score.Attributes
pong = Score.attr "pong"

-- | Closed right hand stroke.
phak :: Score.Attributes
phak = Score.attr "phak"

-- | Kulanter gede, open stroke.
tung :: Score.Attributes
tung = Score.attr "tung"

-- | Kulanter leutik, closed stroke.
pak :: Score.Attributes
pak = Score.attr "pak"

-- | Kulanter leutik, open stroke.
peung :: Score.Attributes
peung = Score.attr "peung"
