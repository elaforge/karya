-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Miscellaneous gong instruments.
module User.Elaforge.Instrument.Kontakt.Gong (
    patches, write_ksp
    , kajar_resolve_errors
) where
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.Attrs as Attrs
import qualified Derive.C.Bali.Gong as Gong
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Instrument.DUtil as DUtil

import qualified Instrument.InstT as InstT
import qualified Midi.Key as Key
import qualified Perform.RealTime as RealTime
import qualified Ui.Meter.Meter as Meter
import qualified User.Elaforge.Instrument.Kontakt.Util as Util

import           Global


patches :: [MidiInst.Patch]
patches = [kajar_patch]

patch :: InstT.Name -> MidiInst.Patch
patch name = MidiInst.named_patch (-24, 24) name []

kajar_patch :: MidiInst.Patch
kajar_patch =
    MidiInst.code #= code $
        CUtil.pitched_drum_patch kajar_pitched_strokes $ patch "kajar"
    where
    code = MidiInst.cmd (CUtil.insert_call CUtil.MidiThru char_to_call)
        <> MidiInst.note_generators generators
    generators = concat
        [ CUtil.drum_calls (map (,config) strokes)
        , [(sym, call) | (_, sym, call) <- kajar_special]
        , [("k", Gong.make_cycle "kajar" (Just (Left "o"))
            (Just (Left Meter.Q)))]
        ]
    config = CUtil.call_config { CUtil._tuning_control = Just "kajar-tune" }
    char_to_call = concat
        [ [(Drums._char s, Drums._name s) | s <- strokes]
        , [(char, sym) | (char, sym, _) <- kajar_special]
        ]
    strokes = map fst kajar_pitched_strokes

kajar_pitched_strokes :: CUtil.PitchedStrokes
(kajar_pitched_strokes, kajar_resolve_errors) =
    CUtil.resolve_strokes 0.35 keymap kajar_strokes

kajar_special :: [(Char, Expr.Symbol, Derive.Generator Derive.Note)]
kajar_special =
    [ ('c', "oo", DUtil.doubled_call "o" "oo" DUtil.After
        (RealTime.seconds 0.09) 0.75)
    , ('f', "o..", c_nruk)
    ]

c_nruk :: Derive.Generator Derive.Note
c_nruk = Gong.nruk_generator Module.instrument "nruk" "Nruktuk on `o`." $
    Sub.inverting $ \args -> do
        gen <- Eval.get_generator "o"
        Eval.apply_generator (Derive.passed_ctx args) gen []

kajar_strokes :: [(Char, Expr.Symbol, Attrs.Attributes, Drums.Group)]
kajar_stops :: [(Drums.Group, [Drums.Group])]
(kajar_stops, kajar_strokes) = (,) stops
    [ ('q', "P", rim <> closed,             s_closed)

    , ('a', "+/", rim <> Attrs.staccato,    s_open)
    , ('z', "+", rim <> open,               s_open)
    , ('s', ".", center <> closed <> soft,  s_closed)
    , ('x', "o", center <> closed,          s_closed)
    -- 'c' is for oo

    -- This is not commonly used.
    , ('v', "c", center <> open,            s_open)
    , ('m', "m", Attrs.damp,                s_closed)
    ]
    where
    rim = Attrs.rim
    center = Attrs.center
    soft = Attrs.soft
    open = Attrs.open
    closed = Attrs.closed
    s_closed = "closed"
    s_open = "open"
    stops = [(s_closed, [s_open])]

keymap :: Map Attrs.Attributes CUtil.KeyswitchRange
keymap = CUtil.make_keymap2 Nothing 24 6 12 Key.c4
    [ [Attrs.center <> Attrs.closed]
    , [Attrs.center <> Attrs.open]
    , [Attrs.rim <> Attrs.closed]
    , [Attrs.rim <> Attrs.open]
    , [Attrs.rim <> Attrs.staccato]
    , [Attrs.damp]
    ]

write_ksp :: IO ()
write_ksp = mapM_ (uncurry Util.write)
    [ ("kajar.ksp.txt",
        Util.drum_mute_ksp "kajar" kajar_pitched_strokes kajar_stops)
    ]
