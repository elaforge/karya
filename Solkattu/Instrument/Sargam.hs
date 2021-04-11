-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
-- | Realize abstract solkattu Notes to sa-relative notes.
module Solkattu.Instrument.Sargam where
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Derive.Expr as Expr
import qualified Derive.ShowVal as ShowVal
import qualified Solkattu.Instrument.ToScore as ToScore
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu

import qualified Perform.Pitch as Pitch
import Global


type SequenceM g = [S.Note g (Solkattu.Note (Realize.Stroke Stroke))]

newtype Attributes = Attributes (Map Text (Maybe Text))
    deriving (Show, Eq)

data Stroke = Stroke {
    _pitch :: !Pitch.Pitch
    -- | These turn into transformers for the note.
    , _attributes :: !(Set Text)
    } deriving (Show, Eq, Ord)

instance Solkattu.Notation Stroke where
    notation (Stroke pitch attrs) =
        Solkattu.textNotation $ Solkattu.notationText pitch
            <> if Set.null attrs then ""
                else Text.intercalate "+" (Set.toList attrs)

instance Pretty Stroke where pretty = Solkattu.notationText

instance Expr.ToExpr Stroke where
    to_expr (Stroke _ attrs) =
        foldr Expr.transform0 (Expr.generator0 "")
            (map Expr.Symbol (Set.toList attrs))

instance Expr.ToExpr (Realize.Stroke Stroke) where to_expr = Realize.toExpr

instance Solkattu.Notation Pitch.Pitch where
    notation (Pitch.Pitch oct degree) = Solkattu.textNotation $
        Solkattu.notationText degree <> case oct of
            3 -> dotBelow
            4 -> ""
            5 -> dotAbove
            _ -> showt oct

-- | Show pitch as parsed by the raga scales.
scorePitch :: Pitch.Pitch -> Text
scorePitch (Pitch.Pitch oct degree) = showt oct <> Solkattu.notationText degree

instance Solkattu.Notation Pitch.Degree where
    notation (Pitch.Degree pc _accs) = Solkattu.textNotation $
        fromMaybe (showt pc) (degrees Vector.!? pc)
        where degrees = Vector.fromList $ map Text.singleton "srgmpdn"

-- COMBINING DOT ABOVE
dotAbove :: Text
dotAbove = "\x0307"

-- COMBINING DOT BELOW
dotBelow :: Text
dotBelow = "\x0323"

pitchCall :: Pitch.Pitch -> Expr.Call Text
pitchCall (Pitch.Pitch oct (Pitch.Degree pc acc)) = Expr.call "pitch" $
    [ShowVal.show_val oct, ShowVal.show_val pc]
        ++ if acc == 0 then [] else [ShowVal.show_val acc]

data Strokes a = Strokes {
    s_::a, r_::a, g_::a, m_::a, p_::a, d_::a, n_::a
    , s::a, r::a, g::a, m::a, p::a, d::a, n::a
    , s1::a, r1::a, g1::a, m1::a, p1::a, d1::a, n1::a
    } deriving (Functor, Show)

strokes :: Strokes Stroke
strokes = (uncurry stroke) <$> Strokes
    { s_ = (oct+0, 0), r_ = (oct+0, 1), g_ = (oct+0, 2), m_ = (oct+0, 3)
    , p_ = (oct+0, 4), d_ = (oct+0, 5), n_ = (oct+0, 6)
    , s  = (oct+1, 0), r  = (oct+1, 1), g  = (oct+1, 2), m  = (oct+1, 3)
    , p  = (oct+1, 4), d  = (oct+1, 5), n  = (oct+1, 6)
    , s1 = (oct+2, 0), r1 = (oct+2, 1), g1 = (oct+2, 2), m1 = (oct+2, 3)
    , p1 = (oct+2, 4), d1 = (oct+2, 5), n1 = (oct+2, 6)
    }
    where oct = 3

stroke :: Pitch.Octave -> Pitch.PitchClass -> Stroke
stroke oct pc = Stroke (Pitch.pitch oct pc) mempty

notes :: Strokes (SequenceM g)
notes = Realize.strokeToSequence <$> strokes

toScore :: ToScore.ToScore Stroke
toScore durStrokes = (noteTrack, [("*", pitchTrack)])
    where
    noteTrack =
        [ (start, dur, ShowVal.show_val expr)
        | (start, dur, Just expr) <-
            zip3 starts durs (map ToScore.toExpr strokes)
        ]
    pitchTrack =
        [ (start, 0, scorePitch (_pitch (Realize._stroke note)))
        | (start, Just note) <- zip starts (map noteOf strokes)
        ]
    (durs, strokes) = unzip durStrokes
    starts = scanl (+) 0 durs

noteOf :: Realize.Note a -> Maybe (Realize.Stroke a)
noteOf (Realize.Note s) = Just s
noteOf _ = Nothing

-- ** transposition

octave :: Pitch.Octave -> Stroke -> Stroke
octave oct s = s { _pitch = Pitch.add_octave oct (_pitch s) }

add :: Pitch.PitchClass -> Stroke -> Stroke
add steps s = s { _pitch = Pitch.add_pc 7 steps (_pitch s) }
