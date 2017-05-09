-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Kendang patches for "Local.Instrument.Kontakt".
module Local.Instrument.Kontakt.KendangBali where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

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
import qualified Derive.Expr as Expr
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

pasang_code :: MidiInst.Code
pasang_code =
    MidiInst.note_transformers [("realize", c_realize_kendang)]
    <> MidiInst.note_generators c_pasang_calls
    <> MidiInst.cmd pasang_cmd

tunggal_notes :: CUtil.PitchedNotes
(tunggal_notes, resolve_errors) =
    CUtil.resolve_strokes 0.3 tunggal_keymap
        [ (char, to_call stroke attrs, attrs, group)
        | (char, stroke, attrs, group) <- tunggal_strokes
        ]

tunggal_keymap :: Map Attrs.Attributes CUtil.KeyswitchRange
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

tunggal_strokes :: [(Char, Stroke, Attrs.Attributes, Drums.Group)]
kendang_stops :: [(Drums.Group, [Drums.Group])]
(kendang_stops, tunggal_strokes) = (,) stops
    [ ('b', Plak, plak,         both)
    -- left
    , ('1', Pak, pak <> soft,   left_closed)
    , ('q', Pak, pak,           left_closed)
    , ('w', Pang, pang,         left_open)
    , ('3', TutL, left <> tut <> soft,  left_open)
    , ('e', TutL, left <> tut,  left_open)
    , ('r', DeL, left <> de,    left_open)
    -- right
    , ('a', De, de <> soft,     right_open)
    , ('z', De, de,             right_open)
    , ('s', De, de <> Attrs.thumb,    right_open)
    , ('d', De, de <> Attrs.staccato, right_open)
    , ('x', Tut, tut,           right_open)
    , ('c', Ka, ka <> soft,     right_closed)
    , ('f', Ka, ka,             right_closed)
    , ('.', Dag, dag,           right_open)
    , ('l', Dag, dag <> soft,   right_open)
    , ('/', Tek, tek,           right_closed)
    , (';', Tek, tek <> soft,   right_closed)
    ]
    where
    left = Attrs.left
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

to_call :: Stroke -> Attrs.Attributes -> BaseTypes.CallId
to_call stroke attrs = Expr.CallId $ case stroke of
    Plak -> "PL"
    -- left
    Pak -> if soft then "^" else "P"
    Pang -> "T"
    TutL -> if soft then "ø" else "Ø"
    DeL -> "`O+`"
    -- right
    Ka -> if soft then "." else ".."
    Tut -> "o"
    De
        | soft -> "-"
        | has Attrs.thumb -> "+."
        | has Attrs.staccato -> "+/"
        | otherwise -> "+"
    Dag -> if soft then "-<" else "<"
    Tek -> if soft then "-[" else "["
    where
    has = Attrs.contain attrs
    soft = Attrs.contain attrs Attrs.soft

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
    make_note attrs =
        Drums.note_dyn char (to_call stroke attrs) attrs
            (if Attrs.contain attrs soft then 0.3 else 1)
        where
        Just (char, stroke, _, _) =
            List.find ((==attrs) . attrs_of) tunggal_strokes
    attrs_of (_, _, a, _) = a

write_ksp :: IO ()
write_ksp = mapM_ (uncurry Util.write)
    [ ("kendang-bali.ksp",
        Util.drum_mute_ksp "kendang bali" tunggal_notes kendang_stops)
    ]

-- * config

-- | @LInst.merge $ KendangBali.allocations ...@
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

data Pasang = Pasang { wadon :: Score.Instrument, lanang :: Score.Instrument }
    deriving (Show)

pasang_env :: Sig.Parser Pasang
pasang_env = Pasang
    <$> Sig.required_environ "wadon" Sig.Unprefixed "Wadon instrument."
    <*> Sig.required_environ "lanang" Sig.Unprefixed "Lanang instrument."

pasang_cmd :: Cmd.M m => Msg.Msg -> m Cmd.Status
pasang_cmd = CUtil.insert_call $ Map.fromList
    [(char, name) | (char, name, _) <- pasang_calls]

c_pasang_calls :: [(BaseTypes.CallId, Derive.Generator Derive.Note)]
c_pasang_calls =
    [ (name, c_pasang_stroke name stroke)
    | (name, stroke) <- map (\(_, a, b) -> (a, b)) pasang_calls ++ both_calls
    ]

c_pasang_stroke :: BaseTypes.CallId -> PasangStroke
    -> Derive.Generator Derive.Note
c_pasang_stroke call_id pstroke = Derive.generator Module.instrument
    (Derive.sym_to_call_name call_id) Tags.inst "Dispatch to wadon or lanang." $
    Sig.call pasang_env call
    where
    call pasang args = case pstroke of
        Wadon stroke -> dispatch wadon stroke
        Lanang stroke -> dispatch lanang stroke
        Both w l -> dispatch wadon w <> dispatch lanang l
        where
        dispatch inst stroke_dyn = Derive.with_instrument (inst pasang) $
            Eval.reapply_generator args (stroke_dyn_to_call stroke_dyn)

both_calls :: [(BaseTypes.CallId, PasangStroke)]
both_calls =
    ("PLPL", Both (Plak, Loud) (Plak, Loud)) :
    [ (wadon ^ lanang, Both wstroke lstroke)
    | (_, wadon, Wadon wstroke) <- pasang_calls
    , (_, lanang, Lanang lstroke) <- pasang_calls
    , fst lstroke /= Plak
    , Both wstroke lstroke `Set.notMember` already_bound
    ]
    where
    already_bound = Set.fromList [stroke | (_, _, stroke) <- pasang_calls]
    a ^ b = Expr.CallId (Expr.uncall a <> Expr.uncall b)

pasang_calls :: [(Char, BaseTypes.CallId, PasangStroke)]
pasang_calls =
    [ ('b', "PL", lanang Plak)
    , ('t', "Ø", lanang TutL)
    , ('5', "ø", Lanang (TutL, Soft))
    , ('y', "+Ø", Both (De, Loud) (TutL, Loud))
    -- left
    , ('q', "k", wadon Pak) -- ka
    , ('w', "P", lanang Pak) -- pak
    , ('e', "t", wadon Pang) -- kam
    , ('r', "T", lanang Pang) -- pang
    -- right
    , ('z', "+", wadon De) -- de
    , ('a', "-", Wadon (De, Soft)) -- de
    , ('x', "o", lanang De) -- tut
    , ('c', "u", wadon Tut) -- kum
    , ('v', "U", lanang Tut) -- pung
    , ('m', "<", wadon Dag) -- dag
    , ('j', "-<", Wadon (Dag, Soft)) -- dag
    , (',', ">", lanang Dag) -- dug
    , ('.', "[", wadon Tek) -- tak
    , ('/', "]", lanang Tek) -- tek
    ]
    where
    wadon stroke = Wadon (stroke, Loud)
    lanang stroke = Lanang (stroke, Loud)

-- | Unicode has some kendang notation, but it's harder to type and I'm not
-- sure if I'll wind up using it.
balinese_pasang_calls :: [(Char, BaseTypes.CallId, PasangStroke)]
balinese_pasang_calls =
    [ ('b', "PL",           wadon Plak)
    , ('t', open_ping,      lanang TutL)
    -- left
    , ('q', closed_plak,    wadon Pak) -- ka
    , ('w', closed_pluk,    lanang Pak) -- pak
    , ('e', open_pang,      wadon Pang) -- kam
    , ('r', open_pung,      lanang Pang) -- pang
    -- right
    , ('z', open_dag,       wadon De) -- de
    , ('a', quiet open_dag, Wadon (De, Soft)) -- de
    , ('x', open_dug,       lanang De) -- tut
    , ('c', closed_tak,     wadon Tut) -- kum
    , ('v', closed_tuk,     lanang Tut) -- pung
    -- TODO since I use the same symbols for with and without panggul, there
    -- needs to be a separate attribute.
    , ('m', open_dag,       wadon Dag) -- dag
    , ('j', quiet open_dag, Wadon (Dag, Soft)) -- dag
    , (',', open_dug,       lanang Dag) -- dug
    , ('.', closed_tak,     wadon Tek) -- tak
    , ('/', closed_tuk,     lanang Tek) -- tek
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
    quiet (Expr.CallId s) = Expr.CallId ("," <> s)
    wadon stroke = Wadon (stroke, Loud)
    lanang stroke = Lanang (stroke, Loud)

c_realize_kendang :: Derive.Transformer Derive.Note
c_realize_kendang = Derive.transformer Module.instrument "realize-kendang"
    (Tags.inst <> Tags.postproc)
    "Realize a composite kendang score into separate lanang and wadon parts."
    $ Sig.callt pasang_env
    $ \pasang _args deriver -> realize_kendang pasang <$> deriver

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


-- * general

data PasangStroke = Wadon (Stroke, Dyn) | Lanang (Stroke, Dyn)
    | Both (Stroke, Dyn) (Stroke, Dyn)
    deriving (Eq, Ord, Show)

data Dyn = Soft | Loud deriving (Show, Ord, Eq)

data Stroke =
    Plak -- both
    | Pak | Pang | TutL | DeL -- left
    | Ka | Tut | De | Dag | Tek -- right
    deriving (Eq, Ord, Show)

stroke_dyn_to_call :: (Stroke, Dyn) -> BaseTypes.CallId
stroke_dyn_to_call (stroke, dyn) =
    to_call stroke (if dyn == Soft then Attrs.soft else mempty)

strokes_of :: PasangStroke -> [(Stroke, Dyn)]
strokes_of pstroke = case pstroke of
    Wadon stroke -> [stroke]
    Lanang stroke -> [stroke]
    Both w l -> [w, l]
