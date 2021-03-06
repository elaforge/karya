-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Instrument definitions for kendang Bali, shared between instruments.
module Cmd.Instrument.KendangBali where
import qualified Data.Set as Set

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream


-- * code

pasang_code :: MidiInst.Code
pasang_code =
    MidiInst.note_transformers [("realize", c_realize_kendang)]
    <> MidiInst.note_generators c_pasang_calls
    <> MidiInst.cmd pasang_cmd

-- * tunggal

-- TODO the kontakt one uses CUtil.resolve_strokes, which also takes a keymap
-- and checks for collisions... but I think I don't need it for im?
tunggal_strokes :: [Drums.Stroke]
tunggal_strokes = do
    (key, note@(Note _ attrs), group) <- tunggal_table
    return $ Drums.Stroke
        { _name = to_call note
        , _attributes = attrs
        , _char = key
        , _dynamic = if Attrs.contain attrs Attrs.soft then soft_dyn else 1
        , _group = group
        }

soft_dyn :: Double
soft_dyn = 0.4

tunggal_table :: [(Char, Note, Drums.Group)]
stops :: Drums.Stops
(stops, tunggal_table) = (stops,) $ map to_note
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
    to_note (key, stroke, attrs, group) = (key, Note stroke attrs, group)
    left = Attrs.left
    soft = Attrs.soft
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

to_call :: Note -> Expr.Symbol
to_call (Note stroke attrs) = Expr.Symbol $ case stroke of
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

-- * pasang

data Pasang = Pasang { wadon :: ScoreT.Instrument, lanang :: ScoreT.Instrument }
    deriving (Show)

pasang_env :: Sig.Parser Pasang
pasang_env = Pasang
    <$> Sig.required_environ "wadon" Sig.Unprefixed "Wadon instrument."
    <*> Sig.required_environ "lanang" Sig.Unprefixed "Lanang instrument."

pasang_cmd :: Cmd.M m => Cmd.Handler m
pasang_cmd = CUtil.insert_call CUtil.MidiThru
    [(char, name) | (char, name, _) <- pasang_calls]

c_pasang_calls :: [(Expr.Symbol, Derive.Generator Derive.Note)]
c_pasang_calls =
    [ (name, c_pasang_stroke name stroke)
    | (name, stroke) <- map (\(_, a, b) -> (a, b)) pasang_calls ++ both_calls
    ]

c_pasang_stroke :: Expr.Symbol -> PasangStroke -> Derive.Generator Derive.Note
c_pasang_stroke sym pstroke = Derive.generator Module.instrument
    (Derive.sym_to_call_name sym) Tags.inst "Dispatch to wadon or lanang." $
    Sig.call pasang_env call
    where
    call pasang args = case pstroke of
        Wadon note -> dispatch wadon note
        Lanang note -> dispatch lanang note
        Both w l -> dispatch wadon w <> dispatch lanang l
        where
        dispatch inst note = Derive.with_instrument (inst pasang) $
            Eval.reapply_generator args (to_call note)

both_calls :: [(Expr.Symbol, PasangStroke)]
both_calls =
    ("PLPL", Both (Note Plak mempty) (Note Plak mempty)) :
    [ (wadon <> lanang, Both wnote lnote)
    | (_, wadon, Wadon wnote) <- pasang_calls
    , (_, lanang, Lanang lnote@(Note lstroke _)) <- pasang_calls
    , lstroke /= Plak
    , Both wnote lnote `Set.notMember` already_bound
    ]
    where
    already_bound = Set.fromList [stroke | (_, _, stroke) <- pasang_calls]

pasang_calls :: [(Char, Expr.Symbol, PasangStroke)]
pasang_calls =
    [ ('b', "PL", lanang Plak)
    , ('t', "Ø", lanang TutL)
    , ('5', "ø", Lanang (Note TutL soft))
    , ('y', "+Ø", Both (Note De mempty) (Note TutL mempty))
    -- left
    , ('q', "k", wadon Pak) -- ka
    , ('w', "P", lanang Pak) -- pak
    , ('e', "t", wadon Pang) -- kam
    , ('r', "T", lanang Pang) -- pang
    -- right
    , ('z', "+", wadon De) -- de
    , ('d', "+/", Wadon (Note De Attrs.staccato))
    , ('a', "-", Wadon (Note De soft)) -- de
    , ('x', "o", lanang De) -- tut
    , ('c', "u", wadon Tut) -- kum
    , ('v', "U", lanang Tut) -- pung
    , ('m', "<", wadon Dag) -- dag
    , ('j', "-<", Wadon (Note Dag soft)) -- dag
    , (',', ">", lanang Dag) -- dug
    , ('.', "[", wadon Tek) -- tak
    , ('/', "]", lanang Tek) -- tek
    ]
    where
    soft = Attrs.soft
    wadon stroke = Wadon (Note stroke mempty)
    lanang stroke = Lanang (Note stroke mempty)

-- | Unicode has some kendang notation, but it's harder to type and I'm not
-- sure if I'll wind up using it.
balinese_pasang_calls :: [(Char, Expr.Symbol, PasangStroke)]
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
    , ('a', quiet open_dag, Wadon (Note De soft)) -- de
    , ('x', open_dug,       lanang De) -- tut
    , ('c', closed_tak,     wadon Tut) -- kum
    , ('v', closed_tuk,     lanang Tut) -- pung
    -- TODO since I use the same symbols for with and without panggul, there
    -- needs to be a separate attribute.
    , ('m', open_dag,       wadon Dag) -- dag
    , ('j', quiet open_dag, Wadon (Note Dag soft)) -- dag
    , (',', open_dug,       lanang Dag) -- dug
    , ('.', closed_tak,     wadon Tek) -- tak
    , ('/', closed_tuk,     lanang Tek) -- tek
    ]
    where
    soft = Attrs.soft
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
    quiet = ("," <>)
    wadon stroke = Wadon (Note stroke mempty)
    lanang stroke = Lanang (Note stroke mempty)

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

data PasangStroke = Wadon !Note | Lanang !Note | Both !Note !Note
    deriving (Eq, Ord, Show)

-- | The attributes might have the stroke, or not, so it might be
-- Note Plak plak, or Note Play mempty.
data Note = Note !Stroke !Attrs.Attributes
    deriving (Eq, Ord, Show)

data Stroke =
    Plak -- both
    | Pak | Pang | TutL | DeL -- left
    | Ka | Tut | De | Dag | Tek -- right
    deriving (Eq, Ord, Show)

notes_of :: PasangStroke -> [Note]
notes_of pstroke = case pstroke of
    Wadon note -> [note]
    Lanang note -> [note]
    Both w l -> [w, l]
