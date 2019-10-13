-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Instrument definitions for mridangam.  These are shared between multiple
-- mridangam definitions.
module Cmd.Instrument.Mridangam where
import           Prelude hiding (min, tan)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.ImInst as ImInst

import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.PSignal as PSignal

import qualified Perform.Pitch as Pitch

import           Global


-- * mridangam

code :: CUtil.Thru -> Pitch.NoteNumber
    -> Maybe (Derive.TransformerF Derive.Note) -> ImInst.Code
code thru natural_nn transform =
    make_code thru pitched_strokes natural_nn transform all_notes both_calls

-- | Single symbols for two strokes together.  thom+x becomes a capital X,
-- and there are a few ad-hoc capital letters for more common tha+x
-- combinations.
both_calls :: [(Expr.Symbol, [Expr.Symbol], Maybe Char)]
both_calls = make_both left_notes right_notes special_names
    [ ("N", 'g'), ("D", 'b')
    , ("K", 'h'), ("T", 'n')
    , ("P", 'j'), ("X", 'm')
    ]
    where
    special_names = [("P", ["*", "k"]), ("X", ["*", "t"])]
        ++ [(sym c, ["o", sym (Char.toLower c)]) | c <- "KTNDUVI"]
    sym = Expr.Symbol . Text.singleton

-- | Strokes which have a pitch, which should change with the sruti.
pitched_strokes :: [Attrs.Attributes]
pitched_strokes =
    [ nam, din
    , kin, tan
    , chapu
    , dheem
    ]

all_notes :: [Drums.Note]
all_notes = left_notes ++ right_notes

-- | The convention is symbols for thoppi, and letters for valantalai.  Also,
-- vowels for open sounds, consonants for closed ones.  Soft strokes look like
-- a simpler version of their equivalent loud strokes.
left_notes, right_notes :: [Drums.Note]
stops :: [(Drums.Group, [Drums.Group])]
(left_notes, right_notes, stops) = (left_notes, right_notes, stops)
    where
    left_notes = concat
        [ group t_closed
            [ n 'a' "-" tha 0.5
            , n 'z' "+" tha 0.75
            , n 'Z' "*" tha 1
            , n 'A' "*_" (tha <> fingers) 1
            -- This often alternates with o_.
            , n 'C' "+_" (tha <> fingertips) 1
            ]
        , group t_open
            [ n 's' "." thom 0.5
            , n 'x' "o" thom 1
            , n 'd' "._" gumki 0.5
            , n 'c' "o_" gumki 1
            , n 'f' "o-" (gumki <> Attrs.medium) 1
            , n 'g' "o^" (gumki <> Attrs.high) 1
            , n 'v' "o/" (gumki <> Attrs.up) 1
            -- TODO when I have samples, have 'o 0' to 'o 1' for arbitrary
            -- pitches.
            ]
        ]
    right_notes = concat
        [ group v_closed
            -- TODO this should be mi, played with middle finger, but I have no
            -- sample for it
            [ n '1' "l" ki 0.5
            , n 'q' "k" ki 1
            , n 'w' "t" ta 1
            ]
        , group v_sadam
            [ n '2' "'" min 1
            , n 'e' "n" nam 1
            , n 'r' "d" din 1
            , n '7' "," kin 1
            , n 'u' "^" tan 1
            ]
        , group v_chapu
            [ n '5' "v" (muru <> chapu) 1
            , n 't' "u" (arai <> chapu) 1
            ]
        , group v_dheem [n 'y' "i" dheem 1]
        ]

    -- each group with the groups it stops
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
    group name = map $ \n -> n { Drums._group = name }
    n = Drums.note_dyn

tha = Attrs.attr "tha"
thom = Attrs.attr "thom"
ki = Attrs.attr "ki"
ta = Attrs.attr "ta"
min = Attrs.attr "min" -- like ta or mi, but on meetu so din rings
nam = Attrs.attr "nam"
din = Attrs.attr "din"
dheem = Attrs.attr "dheem"
chapu = Attrs.attr "chapu"
muru = Attrs.attr "muru"
arai = Attrs.attr "arai"
kin = Attrs.attr "kin"
tan = Attrs.attr "tan"

gumki = Attrs.attr "gumki"

-- tha variations
fingers = Attrs.attr "fingers" -- played with flat fingers, not palm
fingertips = Attrs.attr "fingertips"
-- TODO roll is roll with fingertips?


-- * two-handed pitched drums

-- | Make code for a pitched two-handed drum.  This isn't mridangam-specific.
make_code :: CUtil.Thru -> [Attrs.Attributes] -> Pitch.NoteNumber
    -> Maybe (Derive.TransformerF Derive.Note)
    -> [Drums.Note] -> [(Expr.Symbol, [Expr.Symbol], Maybe Char)] -> ImInst.Code
make_code thru pitched_strokes natural_nn transform notes both = mconcat
    [ ImInst.note_generators generators
    , ImInst.val_calls vals
    , ImInst.cmd (CUtil.insert_call thru char_to_call)
    ]
    where
    add t = map (second (Make.modify_generator_ "" t))
    generators = maybe id add transform $ concat
        [ CUtil.drum_calls (Just (pitched_strokes, natural_nn)) Nothing notes
        , DUtil.multiple_calls [(call, subcalls) | (call, subcalls, _) <- both]
        ]
    vals =
        [ ("natural", Make.constant_val Module.instrument "natural"
            doc (PSignal.nn_pitch natural_nn))
        ]
        where doc = "Emit the drum's recorded pitch. Use like `#=(natural)`."
    char_to_call = Map.fromList $ concat
        [ [(Drums._char n, Drums._name n) | n <- notes]
        , [(char, call) | (call, _, Just char) <- both]
        ]

-- | Create calls for all simultaneous left and right hand combinations, and
-- key bindings for a few common ones.
make_both :: [Drums.Note] -> [Drums.Note]
    -> [(Expr.Symbol, [Expr.Symbol])] -- ^ special names for pairs
    -> [(Expr.Symbol, Char)] -> [(Expr.Symbol, [Expr.Symbol], Maybe Char)]
make_both left right special_names keys =
    [ (call, subcalls, lookup call keys)
    | (call, subcalls) <- special_names ++ pairs
    ]
    where
    pairs =
        [ (Expr.Symbol $ u lcall <> u rcall, [lcall, rcall])
        | lcall <- map Drums._name left
        , rcall <- map Drums._name right
        , Text.length (u lcall) == 1 && Text.length (u rcall) == 1
        ]
    u = Expr.unsym
