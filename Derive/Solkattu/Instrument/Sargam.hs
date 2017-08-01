-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Realize abstract solkattu Notes to sa-relative notes.
module Derive.Solkattu.Instrument.Sargam where
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Derive.Expr as Expr
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Solkattu.Instrument.ToScore as ToScore
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu

import qualified Perform.Pitch as Pitch
import Global


type SNote = Sequence.Note (Realize.Note Stroke)

note :: stroke -> Realize.SNote stroke
note = Sequence.Note . Realize.Note . Realize.stroke

newtype Attributes = Attributes (Map Text (Maybe Text))
    deriving (Show, Eq)

data Stroke = Stroke {
    _pitch :: !Pitch.Pitch
    -- | These turn into transformers for the note.
    , _attributes :: !(Set Text)
    } deriving (Show, Eq, Ord)

instance Pretty Stroke where
    pretty (Stroke pitch attrs) = sargam_pitch pitch
        <> if Set.null attrs then ""
            else Text.intercalate "+" (Set.toList attrs)

instance Expr.ToExpr Stroke where
    to_expr (Stroke _ attrs) =
        foldr Expr.transform0 (Expr.generator0 "")
            (map Expr.Symbol (Set.toList attrs))

instance Expr.ToExpr (Realize.Stroke Stroke) where to_expr = Realize.to_expr

-- | For Realize.format TODO - make a separate typeclass for this
sargam_pitch :: Pitch.Pitch -> Text
sargam_pitch (Pitch.Pitch oct degree) = sargam_degree degree <> case oct of
    3 -> dot_below
    4 -> ""
    5 -> dot_above
    _ -> showt oct

-- | Show pitch as parsed by the raga scales.
score_pitch :: Pitch.Pitch -> Text
score_pitch (Pitch.Pitch oct degree) = showt oct <> sargam_degree degree

sargam_degree :: Pitch.Degree -> Text
sargam_degree (Pitch.Degree pc _accs) =
    fromMaybe (showt pc) (degrees Vector.!? pc)
    where degrees = Vector.fromList $ map Text.singleton "srgmpdn"

-- COMBINING DOT ABOVE
dot_above :: Text
dot_above = "\x0307"

-- COMBINING DOT BELOW
dot_below :: Text
dot_below = "\x0323"

pitch_call :: Pitch.Pitch -> Expr.Call Text
pitch_call (Pitch.Pitch oct (Pitch.Degree pc acc)) = Expr.call "pitch" $
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

notes :: Strokes SNote
notes = note <$> strokes

to_score :: ToScore.ToScore Stroke
to_score dur_strokes = (note_track, [("*", pitch_track)])
    where
    note_track =
        [ (start, dur, ShowVal.show_val expr)
        | (start, dur, Just expr) <- zip3 starts durs (map to_expr strokes)
        ]
    pitch_track =
        [ (start, 0, score_pitch (_pitch (Realize._stroke note)))
        | (start, Just note) <- zip starts (map note_of strokes)
        ]
    (durs, strokes) = unzip dur_strokes
    starts = scanl (+) 0 durs
    to_expr s = case s of
        Realize.Note stroke -> Just $ Expr.to_expr stroke
        Realize.Pattern p -> Just $ Expr.to_expr p
        Realize.Space _ -> Nothing

note_of :: Realize.Note a -> Maybe (Realize.Stroke a)
note_of (Realize.Note s) = Just s
note_of _ = Nothing

-- ** transposition

octave :: Pitch.Octave -> Stroke -> Stroke
octave oct s = s { _pitch = Pitch.add_octave oct (_pitch s) }

add :: Pitch.PitchClass -> Stroke -> Stroke
add steps s = s { _pitch = Pitch.add_pc 7 steps (_pitch s) }

-- * instrument

instrument ::
    [([Sequence.Note (Solkattu.Note Solkattu.Sollu)], [Realize.SNote Stroke])]
    -> Patterns -> Either Text (Realize.Instrument Stroke)
instrument = Realize.instrument (Realize.simple_stroke_map [])

-- * patterns

type Patterns = Realize.Patterns Stroke

patterns :: Either Text (Realize.Patterns Stroke)
patterns = Realize.patterns []
