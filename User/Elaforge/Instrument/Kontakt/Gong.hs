-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Miscellaneous gong instruments.
module User.Elaforge.Instrument.Kontakt.Gong (
    patches, writeKsp
    , kajarResolveErrors
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
patches = [kajarPatch]

patch :: InstT.Name -> MidiInst.Patch
patch name = MidiInst.namedPatch (-24, 24) name []

kajarPatch :: MidiInst.Patch
kajarPatch =
    MidiInst.code #= code $
        CUtil.pitchedDrumPatch kajarPitchedStrokes $ patch "kajar"
    where
    code = MidiInst.cmd (CUtil.insertCall CUtil.MidiThru charToCall)
        <> MidiInst.noteGenerators generators
    generators = concat
        [ CUtil.drumCalls (map (,config) strokes)
        , [(sym, call) | (_, sym, call) <- kajarSpecial]
        , [("k", Gong.makeCycle "kajar" (Just (Left "o"))
            (Just (Left Meter.Q)))]
        ]
    config = CUtil.callConfig { CUtil._tuningControl = Just "kajar-tune" }
    charToCall = concat
        [ [(c, _name) | Drums.Stroke { _char = Just c, _name } <- strokes]
        , [(char, sym) | (char, sym, _) <- kajarSpecial]
        ]
    strokes = map fst kajarPitchedStrokes

kajarPitchedStrokes :: CUtil.PitchedStrokes
(kajarPitchedStrokes, kajarResolveErrors) =
    CUtil.resolveStrokes 0.35 keymap kajarStrokes

kajarSpecial :: [(Char, Expr.Symbol, Derive.Generator Derive.Note)]
kajarSpecial =
    [ ('c', "oo", DUtil.doubledCall "o" "oo" DUtil.After
        (RealTime.seconds 0.09) 0.75)
    , ('f', "o..", cNruk)
    ]

cNruk :: Derive.Generator Derive.Note
cNruk = Gong.nrukGenerator Module.instrument "nruk" "Nruktuk on `o`." $
    Sub.inverting $ \args -> do
        gen <- Eval.getGenerator "o"
        Eval.applyGenerator (Derive.passedCtx args) gen []

kajarStrokes :: [(Char, Expr.Symbol, Attrs.Attributes, Drums.Group)]
kajarStops :: [(Drums.Group, [Drums.Group])]
(kajarStops, kajarStrokes) = (,) stops
    [ ('q', "P", rim <> closed,             sClosed)

    , ('a', "+/", rim <> Attrs.staccato,    sOpen)
    , ('z', "+", rim <> open,               sOpen)
    , ('s', ".", center <> closed <> soft,  sClosed)
    , ('x', "o", center <> closed,          sClosed)
    -- 'c' is for oo

    -- This is not commonly used.
    , ('v', "c", center <> open,            sOpen)
    , ('m', "m", Attrs.damp,                sClosed)
    ]
    where
    rim = Attrs.rim
    center = Attrs.center
    soft = Attrs.soft
    open = Attrs.open
    closed = Attrs.closed
    sClosed = "closed"
    sOpen = "open"
    stops = [(sClosed, [sOpen])]

keymap :: Map Attrs.Attributes CUtil.KeyswitchRange
keymap = CUtil.makeKeymap2 Nothing 24 6 12 Key.c4
    [ [Attrs.center <> Attrs.closed]
    , [Attrs.center <> Attrs.open]
    , [Attrs.rim <> Attrs.closed]
    , [Attrs.rim <> Attrs.open]
    , [Attrs.rim <> Attrs.staccato]
    , [Attrs.damp]
    ]

writeKsp :: IO ()
writeKsp = mapM_ (uncurry Util.write)
    [ ("kajar.ksp.txt",
        Util.drumMuteKsp "kajar" kajarPitchedStrokes kajarStops)
    ]
