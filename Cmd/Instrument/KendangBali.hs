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

pasangCode :: MidiInst.Code
pasangCode =
    MidiInst.noteTransformers [("realize", cRealizeKendang)]
    <> MidiInst.noteGenerators cPasangCalls
    <> MidiInst.cmd pasangCmd

-- * tunggal

-- TODO the kontakt one uses CUtil.resolve_strokes, which also takes a keymap
-- and checks for collisions... but I think I don't need it for im?
tunggalStrokes :: [Drums.Stroke]
tunggalStrokes = do
    (key, note@(Note _ attrs), group) <- tunggalTable
    pure $ Drums.Stroke
        { _name = toCall note
        , _attributes = attrs
        , _char = if key == ' ' then Nothing else Just key
        , _dynamic = if Attrs.contain attrs Attrs.soft then softDyn else 1
        , _group = group
        }

softDyn :: Double
softDyn = 0.4

tunggalTable :: [(Char, Note, Drums.Group)]
stops :: Drums.Stops
(stops, tunggalTable) = (stops,) $ map toNote
    [ ('b', Plak, plak,         both)
    -- left
    , ('1', Pak, pak <> soft,   leftClosed)
    , ('q', Pak, pak,           leftClosed)
    , ('w', Pang, pang,         leftOpen)
    , ('3', TutL, left <> tut <> soft,  leftOpen)
    , ('e', TutL, left <> tut,  leftOpen)
    , ('r', DeL, left <> de,    leftOpen)
    -- right
    , ('a', De, de <> soft,     rightOpen)
    , ('z', De, de,             rightOpen)
    , ('s', De, de <> Attrs.thumb,    rightOpen)
    , ('d', De, de <> Attrs.staccato, rightOpen)
    , ('x', Tut, tut,           rightOpen)
    , ('c', Ka, ka <> soft,     rightClosed)
    , ('f', Ka, ka,             rightClosed)
    , ('.', Dag, dag,           rightOpen)
    , ('l', Dag, dag <> soft,   rightOpen)
    , ('/', Tek, tek,           rightClosed)
    , (';', Tek, tek <> soft,   rightClosed)
    ]
    where
    toNote (key, stroke, attrs, group) = (key, Note stroke attrs, group)
    left = Attrs.left
    soft = Attrs.soft
    stops =
        [ (both, [leftOpen, rightOpen])
        , (leftClosed, [leftOpen])
        , (rightClosed, [rightOpen])
        ]
    both = "both"
    leftClosed = "left-closed"
    leftOpen = "left-open"
    rightClosed = "right-closed"
    rightOpen = "right-open"

toCall :: Note -> Expr.Symbol
toCall (Note stroke attrs) = Expr.Symbol $ case stroke of
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

pasangEnv :: Sig.Parser Pasang
pasangEnv = Pasang
    <$> Sig.requiredEnviron "wadon" Sig.Unprefixed "Wadon instrument."
    <*> Sig.requiredEnviron "lanang" Sig.Unprefixed "Lanang instrument."

pasangCmd :: Cmd.M m => Cmd.Handler m
pasangCmd = CUtil.insertCall CUtil.MidiThru
    [(char, name) | (char, name, _) <- pasangCalls]

cPasangCalls :: [(Expr.Symbol, Derive.Generator Derive.Note)]
cPasangCalls =
    [ (name, cPasangStroke name stroke)
    | (name, stroke) <- map (\(_, a, b) -> (a, b)) pasangCalls ++ bothCalls
    ]

cPasangStroke :: Expr.Symbol -> PasangStroke -> Derive.Generator Derive.Note
cPasangStroke sym pstroke = Derive.generator Module.instrument
    (Derive.symToCallName sym) Tags.inst "Dispatch to wadon or lanang." $
    Sig.call pasangEnv call
    where
    call pasang args = case pstroke of
        Wadon note -> dispatch wadon note
        Lanang note -> dispatch lanang note
        Both w l -> dispatch wadon w <> dispatch lanang l
        where
        dispatch inst note = Derive.withInstrument (inst pasang) $
            Eval.reapplyGenerator args (toCall note)

bothCalls :: [(Expr.Symbol, PasangStroke)]
bothCalls =
    ("PLPL", Both (Note Plak mempty) (Note Plak mempty)) :
    [ (wadon <> lanang, Both wnote lnote)
    | (_, wadon, Wadon wnote) <- pasangCalls
    , (_, lanang, Lanang lnote@(Note lstroke _)) <- pasangCalls
    , lstroke /= Plak
    , Both wnote lnote `Set.notMember` alreadyBound
    ]
    where
    alreadyBound = Set.fromList [stroke | (_, _, stroke) <- pasangCalls]

pasangCalls :: [(Char, Expr.Symbol, PasangStroke)]
pasangCalls =
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
balinesePasangCalls :: [(Char, Expr.Symbol, PasangStroke)]
balinesePasangCalls =
    [ ('b', "PL",           wadon Plak)
    , ('t', openPing,      lanang TutL)
    -- left
    , ('q', closedPlak,    wadon Pak) -- ka
    , ('w', closedPluk,    lanang Pak) -- pak
    , ('e', openPang,      wadon Pang) -- kam
    , ('r', openPung,      lanang Pang) -- pang
    -- right
    , ('z', openDag,       wadon De) -- de
    , ('a', quiet openDag, Wadon (Note De soft)) -- de
    , ('x', openDug,       lanang De) -- tut
    , ('c', closedTak,     wadon Tut) -- kum
    , ('v', closedTuk,     lanang Tut) -- pung
    -- TODO since I use the same symbols for with and without panggul, there
    -- needs to be a separate attribute.
    , ('m', openDag,       wadon Dag) -- dag
    , ('j', quiet openDag, Wadon (Note Dag soft)) -- dag
    , (',', openDug,       lanang Dag) -- dug
    , ('.', closedTak,     wadon Tek) -- tak
    , ('/', closedTuk,     lanang Tek) -- tek
    ]
    where
    soft = Attrs.soft
    -- left
    openPang = "᭸"     -- t kam
    openPung = "᭹"     -- T pang
    closedPlak = "᭺"   -- k ka
    closedPluk = "᭻"   -- P pak
    openPing = "᭼"     -- Ø pung
    -- right
    openDag = "᭵"      -- < dag   + de
    openDug = "᭴"      -- > dug   o tut
    closedTak = "᭷"    -- ] tek   u kum
    closedTuk = "᭶"    -- [ tak   U pung
    quiet = ("," <>)
    wadon stroke = Wadon (Note stroke mempty)
    lanang stroke = Lanang (Note stroke mempty)

cRealizeKendang :: Derive.Transformer Derive.Note
cRealizeKendang = Derive.transformer Module.instrument "realize-kendang"
    (Tags.inst <> Tags.postproc)
    "Realize a composite kendang score into separate lanang and wadon parts."
    $ Sig.callt pasangEnv
    $ \pasang _args deriver -> realizeKendang pasang <$> deriver

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
realizeKendang :: Pasang -> Stream.Stream Score.Event
    -> Stream.Stream Score.Event
realizeKendang _pasang events = events -- TODO


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

data Stroke
    = Plak -- both
    | Pak | Pang | TutL | DeL -- left
    | Ka | Tut | De | Dag | Tek -- right
    deriving (Eq, Ord, Show)

notesOf :: PasangStroke -> [Note]
notesOf pstroke = case pstroke of
    Wadon note -> [note]
    Lanang note -> [note]
    Both w l -> [w, l]
