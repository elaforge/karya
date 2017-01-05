-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Kendang patches for "Local.Instrument.Kontakt".
module Local.Instrument.Kontakt.KendangBali where
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Ui.UiConfig as UiConfig
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Msg as Msg

import qualified Derive.Attrs as Attrs
import Derive.Attrs (soft)
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream

import qualified Perform.Midi.Patch as Patch
import qualified Instrument.Common as Common
import qualified Local.Instrument.Kontakt.Util as Util
import Global


patches :: [MidiInst.Patch]
patches =
    [ MidiInst.code #= tunggal_code $ CUtil.pitched_drum_patch tunggal_notes $
        patch "kendang-bali"
    , MidiInst.code #= tunggal_code $ CUtil.drum_patch old_tunggal_notes $
        patch "kendang-bali-old"
    , MidiInst.code #= pasang_code $
        MidiInst.patch %= MidiInst.add_flag Patch.Triggered $
        patch "kendang-bali-pasang"
    ]
    where
    tunggal_code = CUtil.drum_code (Just "kendang-tune") (map fst tunggal_notes)
    patch name = MidiInst.named_patch (-24, 24) name []

tunggal_notes :: CUtil.PitchedNotes
(tunggal_notes, resolve_errors) =
    CUtil.resolve_strokes 0.3 tunggal_keymap tunggal_strokes

tunggal_keymap :: Map.Map Attrs.Attributes CUtil.KeyswitchRange
tunggal_keymap = CUtil.make_keymap (Just Key2.e_2) Key2.c_1 12 Key.fs3
    [ [de <> Attrs.staccato, plak]
    , [de <> Attrs.thumb, dag <> Attrs.staccato]
    , [de, dag]
    , [de <> Attrs.closed, tek]
    , [tut]
    , [ka]
    , [pang]
    , [pak]
    , [de <> Attrs.left, tut <> Attrs.left]
    ]

tunggal_strokes :: [(Char, BaseTypes.CallId, Attrs.Attributes, Drums.Group)]
kendang_stops :: [(Drums.Group, [Drums.Group])]
(kendang_stops, tunggal_strokes) = (,) stops
    [ ('b', "PL", plak,                 both)
    -- left
    , ('q', "P", pak,                   left_closed)
    , ('w', "T", pang,                  left_open)
    , ('1', "^", pak <> soft,           left_closed)
    , ('e', "Ø", tut <> Attrs.left,     left_open)
    , ('r', "`O+`", de <> Attrs.left,   left_open)
    -- right
    , ('z', "+", de,                    right_open)
    , ('a', "-", de <> soft,            right_open)
    , ('s', "+.", de <> Attrs.thumb,    right_open)
    , ('d', "+/", de <> Attrs.staccato, right_open)
    , ('x', "o", tut,                   right_open)
    , ('c', ".", ka <> soft,            right_closed)
    , ('f', "..", ka,                   right_closed)
    , ('.', "<", dag,                   right_open)
    , ('l', "-<", dag <> soft,          right_open)
    , ('/', "[", tek,                   right_closed)
    , (';', "-[", tek <> soft,          right_closed)
    ]
    where
    stops =
        [ (both, [left_open, right_open])
        , (left_closed, [left_open])
        , (right_closed, [right_open])
        ]
    both = "both"
    left_closed = "left-closed"
    left_open = "left-open"
    right_closed = "right-closed"
    right_open = "right-open"

-- | Mapping for the old kendang patches.
old_tunggal_notes :: [(Drums.Note, Midi.Key)]
old_tunggal_notes = map (first make_note)
    [ (plak, Key.g1)
    -- left
    , (pak, Key.c5)
    , (pang, Key.g4)
    , (pak <> soft, Key.c5)
    , (de <> Attrs.left, Key.d4)
    , (tut <> Attrs.left, Key.c4)
    -- right
    , (de, Key.c2)
    , (de <> soft, Key.c2)
    , (de <> Attrs.thumb, Key.f2)
    , (de <> Attrs.staccato, Key.c1)
    , (tut, Key.c3)
    , (ka <> soft, Key.g3)
    , (ka, Key.g3)
    , (dag, Key.c2)
    , (dag <> soft, Key.c2)
    , (tek <> soft, Key.c1)
    , (tek, Key.c1)
    ]
    where
    make_note attrs = Drums.note_dyn char call attrs
            (if Attrs.contain attrs soft then 0.3 else 1)
        where
        Just (char, call, _, _) =
            List.find ((==attrs) . attrs_of) tunggal_strokes
    attrs_of (_, _, a, _) = a

write_ksp :: IO ()
write_ksp = mapM_ (uncurry Util.write)
    [ ("kendang-bali.ksp",
        Util.drum_mute_ksp "kendang bali" tunggal_notes kendang_stops)
    ]

-- * config

-- | @LInst.merge $ KontaktKendang.allocations ...@
allocations :: Text -> Text -> UiConfig.Allocations
allocations name dev_ = MidiInst.allocations
    [ (name <> "-w", "kontakt/kendang-bali", id, midi_channel 0)
    , (name <> "-l", "kontakt/kendang-bali", id, midi_channel 1)
    , ( name, "kontakt/kendang-bali-pasang"
      , Common.add_environ "w" (inst $ name <> "-w")
        . Common.add_environ "l" (inst $ name <> "-l")
      , UiConfig.Dummy
      )
    ]
    where
    midi_channel = UiConfig.Midi . MidiInst.config1 dev
    dev = Midi.write_device dev_
    inst = Score.Instrument

-- * pasang

data Kendang = Wadon | Lanang deriving (Show, Eq)
type Pasang = (Score.Instrument, Score.Instrument)

pasang_inst :: Kendang -> Pasang -> Score.Instrument
pasang_inst Wadon = fst
pasang_inst Lanang = snd

-- | (keybinding, call_name, Kendang, dispatch_to_call)
--
-- The dispatch calls should all be understood by a kendang tunggal, i.e.
-- in 'tunggal_strokes'.
pasang_calls :: [(Char, BaseTypes.CallId, Kendang, BaseTypes.CallId)]
pasang_calls =
    [ ('b', "PL", Wadon, "PL")
    , ('t', "Ø", Lanang, "Ø")
    -- left
    , ('q', "k", Wadon, "P") -- ka
    , ('w', "P", Lanang, "P") -- pak
    , ('e', "t", Wadon, "T") -- kam
    , ('r', "T", Lanang, "T") -- pang
    -- right
    , ('z', "+", Wadon, "+") -- de
    , ('a', "-", Wadon, "-") -- de
    , ('x', "o", Lanang, "+") -- tut
    , ('c', "u", Wadon, "o") -- kum
    , ('v', "U", Lanang, "o") -- pung
    , ('m', "<", Wadon, "<") -- dag
    , ('j', "-<", Wadon, "-<") -- dag
    , (',', ">", Lanang, "<") -- dug
    , ('.', "[", Wadon, "[") -- tak
    , ('/', "]", Lanang, "[") -- tek
    ]

-- | Unicode has some kendang notation, but it's harder to type and I'm not
-- sure if I'll wind up using it.
balinese_pasang_calls :: [(Char, BaseTypes.CallId, Kendang, BaseTypes.CallId)]
balinese_pasang_calls =
    [ ('b', "PL",           Wadon, "PL")
    , ('t', open_ping,      Lanang, "Ø")
    -- left
    , ('q', closed_plak,    Wadon,  "P") -- ka
    , ('w', closed_pluk,    Lanang, "P") -- pak
    , ('e', open_pang,      Wadon,  "T") -- kam
    , ('r', open_pung,      Lanang, "T") -- pang
    -- right
    , ('z', open_dag,       Wadon,  "+") -- de
    , ('a', quiet open_dag, Wadon,  "-") -- de
    , ('x', open_dug,       Lanang, "+") -- tut
    , ('c', closed_tak,     Wadon,  "o") -- kum
    , ('v', closed_tuk,     Lanang, "o") -- pung
    -- TODO since I use the same symbols for with and without panggul, there
    -- needs to be a separate attribute.
    , ('m', open_dag,       Wadon,  "<") -- dag
    , ('j', quiet open_dag, Wadon,  "-<") -- dag
    , (',', open_dug,       Lanang, "<") -- dug
    , ('.', closed_tak,     Wadon,  "[") -- tak
    , ('/', closed_tuk,     Lanang, "[") -- tek
    ]
    where
    -- left
    open_pang = "᭸"     -- t kam
    open_pung = "᭹"     -- T pang
    closed_plak = "᭺"   -- k ka
    closed_pluk = "᭻"   -- P pak
    open_ping = "᭼"     -- Ø pung
    -- right
    open_dag = "᭵"      -- < dag   + de
    open_dug = "᭴"      -- > dug   o tut
    closed_tak = "᭷"    -- ] tek   u kum
    closed_tuk = "᭶"    -- [ tak   U pung
    quiet (BaseTypes.Symbol s) = BaseTypes.Symbol ("," <> s)

pasang_code :: MidiInst.Code
pasang_code =
    MidiInst.note_transformers [("realize", c_realize_kendang)]
    <> MidiInst.note_generators c_pasang_calls
    <> MidiInst.cmd pasang_cmd

pasang_cmd :: Cmd.M m => Msg.Msg -> m Cmd.Status
pasang_cmd = CUtil.insert_call $ Map.fromList
    [(char, name) | (char, name, _, _) <- pasang_calls]

c_pasang_calls :: [(BaseTypes.CallId, Derive.Generator Derive.Note)]
c_pasang_calls =
    [(name, dispatch kendang call) | (_, name, kendang, call) <- pasang_calls]

-- | Create a call that just dispatches to another call, possibly transformed.
dispatch :: Kendang -> BaseTypes.CallId -> Derive.Generator Derive.Note
dispatch kendang call = Derive.generator Module.instrument name Tags.inst
    "Dispatch to wadon or lanang." $ Sig.call pasang_env $ \pasang args ->
        Derive.with_instrument (pasang_inst kendang pasang) $
        Eval.reapply_generator args call
    where name = Derive.CallName $ showt kendang <> " " <> pretty call

c_realize_kendang :: Derive.Transformer Derive.Note
c_realize_kendang = Derive.transformer Module.instrument "realize-kendang"
    (Tags.inst <> Tags.postproc)
    "Realize a composite kendang score into separate lanang and wadon parts."
    $ Sig.callt pasang_env
    $ \pasang _args deriver -> realize_kendang pasang <$> deriver

pasang_env :: Sig.Parser Pasang
pasang_env = (,)
    <$> Sig.required_environ "wadon" Sig.Unprefixed "Wadon instrument."
    <*> Sig.required_environ "lanang" Sig.Unprefixed "Lanang instrument."

{- | Given a composite part with lanang and wadon, fill in the secondary
    strokes.

    The realization is not correct because I don't yet fully understand how it
    works.

    > c kPtTtT+o+oo-+
    > l .P.TPTP+^++.^
    > w P.TPTP+.+.^-+

    > c kPktT t T T t T .kP.tT.tTØØØ
    > l .P.^T P T T P T .^P^.T .TØØØ
    > w P^.TP T P P T P .P^.TP.TP. .

    > c kP+otT kPkP+o+o kPuUtT+o
    > l P.+.T^ P.P.+.+. P.o.T^+.
    > w .P.+.T .P.P.+.+ .P.O.T^+

    > c kPtTtT
    > l .P.TPTP
    > w P.TPTP

    > tTkPtTkP
    > T.P.T.P
    > .T.P.T.P

    > tT+otT+o
    > TP+.TP+.
    > .TP+.TP+
-}
realize_kendang :: Pasang -> Stream.Stream Score.Event
    -> Stream.Stream Score.Event
realize_kendang _pasang events = events -- TODO


-- * attrs

-- Kendang tunggal strokes don't really have names so I made some up.
-- For composite it would be: de tut, kum pung, ka pak, kam pang
-- If I took the wadon or lanang names, it would be de, kum, ka, kam, or
-- tut, pung, pak, pang, which both sound weird.

-- both
plak = Attrs.attr "plak"

-- right
de = Attrs.attr "de"
tut = Attrs.attr "tut"
ka = Attrs.attr "ka" -- neutral stroke
dag = Attrs.attr "dag" -- de with panggul
tek = Attrs.attr "tek"

-- left
pak = Attrs.attr "pak"
pang = Attrs.attr "pang" -- rim
