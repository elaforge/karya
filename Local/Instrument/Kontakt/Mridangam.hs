-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Mridangam patch.
module Local.Instrument.Kontakt.Mridangam where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.Attrs as Attrs
import qualified Derive.Instrument.DUtil as DUtil
import Derive.Score (attr)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.NN as NN
import qualified Local.Instrument.Kontakt.Util as Util
import Global


patches :: [MidiInst.Patch]
patches =
    [ (patch "mridangam2" pitched_notes, code)
    , (patch "mridangam" pitched_notes_old, code)
    ]
    where
    patch name notes = CUtil.pitched_drum_patch notes $
        Instrument.patch $ Instrument.instrument name [] pb_range
    code = make_code all_notes both_calls

make_code :: [Drums.Note]
    -> [(TrackLang.CallId, [TrackLang.CallId], Maybe Char)] -> MidiInst.Code
make_code notes both =
    MidiInst.note_generators call_code
        <> MidiInst.cmd (CUtil.insert_call char_to_call)
    where
    call_code = concat
        [ CUtil.drum_calls Nothing notes
        , DUtil.multiple_calls
            [(call, subcalls) | (call, subcalls, _) <- both]
        ]
    char_to_call = Map.fromList $ concat
        [ [(Drums.note_char n, Drums.note_name n) | n <- notes]
        , [(char, call) | (call, _, Just char) <- both]
        ]

pb_range :: Instrument.PbRange
pb_range = (-24, 24)

both_calls :: [(TrackLang.CallId, [TrackLang.CallId], Maybe Char)]
both_calls = make_both left_notes right_notes special_names
    [ ("N", 'f'), ("D", 'v')
    , ("K", 'g'), ("T", 'b')
    , ("P", 'h'), ("X", 'n')
    ]
    where
    special_names = [("P", ["+", "k"]), ("X", ["+", "t"])]
        ++ [(sym c, ["o", sym (Char.toLower c)]) | c <- "KTNDAUVI"]
    sym = TrackLang.Symbol . Text.singleton

-- | Create calls for all simultaneous left and right hand combinations, and
-- key bindings for a few common ones.
make_both :: [Drums.Note] -> [Drums.Note]
    -> [(TrackLang.CallId, [TrackLang.CallId])] -- ^ special names for pairs
    -> [(TrackLang.CallId, Char)]
    -> [(TrackLang.CallId, [TrackLang.CallId], Maybe Char)]
make_both left right special_names keys =
    [ (call, subcalls, lookup call keys)
    | (call, subcalls) <- special_names ++ pairs
    ]
    where
    pairs =
        [ (TrackLang.Symbol $ u rcall <> u lcall, [rcall, lcall])
        | lcall <- map Drums.note_name left
        , rcall <- map Drums.note_name right
        , Text.length (u lcall) == 1
        , Text.length (u rcall) == 1
        ]
    u = TrackLang.unsym

-- | The convention is symbols for thoppi, and letters for valantalai.  Also,
-- vowels for open sounds, consonants for closed ones.  Soft strokes look like
-- a simpler version of their equivalent loud strokes.
left_notes, right_notes :: [Drums.Note]
stops :: [(Drums.Group, [Drums.Group])]
(left_notes, right_notes, stops) = (left_notes, right_notes, stops)
    where
    left_notes = concat $
        [ group t_closed
            [ n 'a' "-" tha 0.5
            , n 'z' "+" tha 1
            ]
        , group t_open
            [ n 's' "." thom 0.5
            , n 'x' "o" thom 1
            , n 'd' "o." gumki 1
            , n 'c' "o^" (gumki <> Attrs.up) 1
            -- later I can have o_ and o- for low and high
            -- or maybe 'o 0' 'o .5' 'o 1'
            ]
        ]
    right_notes = concat $
        [ group v_closed
            [ n '1' "l" ki 0.5
            , n 'q' "k" ki 1
            , n 'w' "t" ta 1
            ]
        , group v_sadam
            [ n 'e' "n" nam 1
            , n 'r' "d" din 1
            , n '7' "," (meetu <> ki) 1
            , n 'u' "^" (meetu <> ta) 1
            ]
        , group v_chapu
            [ n '5' "v" muru 1
            , n 't' "u" arai 1
            ]
        , group v_dheem [n 'y' "i" dheem 1]
        ]

    stops =
        [ (t_closed, [t_open])
        , (v_closed, [v_sadam, v_chapu, v_dheem])
        , (v_sadam, [v_chapu, v_dheem])
        , (v_chapu, [v_dheem])
        ]
    v_closed = "v-closed"
    v_sadam = "v-sadam"
    v_chapu = "v-chapu"
    v_dheem = "v-dheem"
    t_closed = "t-closed"
    t_open = "t-open"
    group name = map $ \n -> n { Drums.note_group = name }
    n = Drums.note_dyn

all_notes :: [Drums.Note]
all_notes = left_notes ++ right_notes

{- | Layout:

    > 0         10        20        30        40        50        60        70        80        90        100       110       120    127
    > 01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
    > c-2         c-1         c0          c1          c2          c3          c4          c5          c6          c7          c8     g8
    >         XXXXtha -------|thom ------|ki --------|ta --------|nam -------|din -------|arai ------|dheem -----|meetu -----|
-}
pitched_notes :: CUtil.PitchedNotes
(pitched_notes, _pitched_notes) = CUtil.drum_pitched_notes all_notes $
    CUtil.make_keymap Key2.g_2 Key2.c_1 12 NN.c4
    [ [tha]
    , [thom, gumki]
    , [ki, gumki <> Attrs.up]
    , [ta]
    , [nam]
    , [din]
    , [arai, muru]
    , [meetu <> ki, dheem, dheem <> Attrs.staccato]
    , [meetu <> ta]
    ]

write_ksp :: IO ()
write_ksp = mapM_ (uncurry Util.write)
    [ ("mridangam.ksp.txt", Util.drum_mute_ksp "mridangam" pitched_notes stops)
    , ("mridangam-old.ksp.txt", Util.drum_mute_ksp "mridangam"
        pitched_notes_old stops)
    ]

pitched_notes_old :: CUtil.PitchedNotes
(pitched_notes_old, _pitched_notes_old) =
    CUtil.drum_pitched_notes all_notes $ map make
    -- left
    [ (tha, (Key.g_1, Key.e0))
    , (thom, (Key.g0, Key.e1))
    , (thom <> Attrs.staccato, (Key.g1, Key.e2))
    -- right
    , (ki, (Key.g2, Key.e3))
    , (ta, (Key.g3, Key.e4))
    , (nam, (Key.g4, Key.e5))
    , (din, (Key.g5, Key.e6))
    , (din <> Attrs.v2, (Key.g6, Key.e7))
    , (dheem, (Key.g7, Key.e8))
    , (arai, (Key.g8, Key.e9))
    , (muru, (Key.g9, Key.g9))
    ]
    where make (attrs, (low, high)) = (attrs, (Nothing, low, high, NN.e3))

-- * attrs

tha = attr "tha"
thom = attr "thom"
ki = attr "ki"
ta = attr "ta"
nam = attr "nam"
din = attr "din"
dheem = attr "dheem"
arai = attr "arai"
muru = attr "muru"
meetu = attr "meetu"

gumki = attr "gumki"
