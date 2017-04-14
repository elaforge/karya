-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Realize an abstract solkattu 'S.Sequence' to concrete kendang 'Note's.
module Derive.Solkattu.KendangTunggal where
import qualified Data.Map as Map
import qualified Util.Pretty as Pretty
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as S
import qualified Derive.Solkattu.Solkattu as Solkattu
import Global


type Note = Realize.Note Stroke

data Stroke =
    Plak -- both
    | Pak | Pang | TutL | DagL -- left
    | Ka | Tut | Dag -- right
    deriving (Eq, Ord, Show)

instrument :: [([Solkattu.Note Stroke], [Note])] -> Patterns
    -> Either Text (Realize.Instrument Stroke)
instrument = Realize.instrument standard_stroke_map

standard_stroke_map :: Realize.StrokeMap Stroke
standard_stroke_map = Realize.StrokeMap $ Map.fromList
    [ ([Solkattu.Thom], [Just Dag])
    , ([Solkattu.Tam], [Just TutL])
    , ([Solkattu.Tang], [Just TutL])
    , ([Solkattu.Lang], [Just TutL])
    , ([Solkattu.Dheem], [Just Dag])
    ]

-- * strokes

instance Pretty.Pretty Stroke where
    pretty s = case s of
        Plak -> "P"
        Pak -> "p"
        Pang -> "t"
        TutL -> "u"
        DagL -> "å"
        Ka -> "k"
        Tut -> "o"
        Dag -> "a"

-- | TODO should I make these consistent?
stroke_to_call :: Stroke -> Text
stroke_to_call s = case s of
    Plak -> "PL"
    Pak -> "P"
    Pang -> "T"
    TutL -> "Ø"
    DagL -> "`O+`"
    Ka -> ".."
    Tut -> "o"
    Dag -> "+"

data Strokes a = Strokes {
    pk :: a, p :: a, t :: a, u :: a, å :: a, k :: a, o :: a , a :: a
    } deriving (Show)

stroke :: stroke -> Realize.Note stroke
stroke = S.Note . Realize.Stroke

strokes :: Strokes Note
strokes = Strokes
    { pk = stroke Plak
    , p = stroke Pak
    , t = stroke Pang
    , u = stroke TutL
    , å = stroke DagL
    , k = stroke Ka
    , o = stroke Tut
    , a = stroke Dag
    }


-- * Patterns

type Patterns = Realize.Patterns Stroke

__ :: Note
__ = S.Note Realize.Rest

defaults :: Patterns
defaults = Solkattu.check $ Realize.patterns
    [ (5, [o, p, k, t, a])
    , (6, [o, p, __, k, t, a])
    , (7, [o, __, p, __, k, t, a])
    , (8, [o, p, __, k, __, t, __, a])
    , (9, [o, __, p, __, k, __, t, __, a])
    ]
    where Strokes {..} = strokes

nakatiku :: [Note]
nakatiku = [t, o, u, k, p, k, a, k]
    where Strokes {..} = strokes
