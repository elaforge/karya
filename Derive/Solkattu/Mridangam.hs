-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Realize an abstract solkattu 'S.Sequence' to concrete mridangam 'Note's.
module Derive.Solkattu.Mridangam where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Pretty as Pretty
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Solkattu as S
import Global


type Note = Realize.Note Stroke

data Stroke = Thoppi !Thoppi | Valantalai !Valantalai | Both !Thoppi !Valantalai
    deriving (Eq, Show)
data Thoppi = Tha | Thom
    deriving (Eq, Show)
data Valantalai = Ki | Ta | Nam | Din | Chapu | Dheem
    deriving (Eq, Show)

instrument :: [(S.Sequence Stroke, [Note])] -> Patterns
    -> Either Text (Realize.Instrument Stroke)
instrument = Realize.instrument standard_stroke_map

standard_stroke_map :: Realize.StrokeMap Stroke
standard_stroke_map = Realize.StrokeMap $ Map.fromList
    [ ([S.Thom], [Just $ Thoppi Thom])
    , ([S.Tam], [Just $ Valantalai Chapu])
    , ([S.Tang], [Just $ Valantalai Chapu])
    , ([S.Lang], [Just $ Valantalai Chapu])
    , ([S.Dheem], [Just $ Valantalai Dheem])
    ]

-- * strokes

instance Pretty.Pretty Stroke where
    pretty (Thoppi t) = pretty t
    pretty (Valantalai v) = pretty v
    pretty (Both t v) = case t of
        Tha -> case v of
            Ki -> "P"
            Ta -> "X"
            Nam -> "A"
            Din -> "O"
            Chapu -> "pu" -- These are pretty rare.
            Dheem -> "pi"
        Thom -> Text.toUpper (pretty v)

instance Pretty.Pretty Thoppi where
    pretty n = case n of
        Thom -> "o"
        Tha -> "p"

instance Pretty.Pretty Valantalai where
    pretty n = case n of
        Ki -> "k"
        Ta -> "t"
        Nam -> "n"
        Din -> "d"
        Chapu -> "u"
        Dheem -> "i"

-- | Pretty reproduces the SolkattuScore syntax, which has to be haskell
-- syntax, so it can't use +, and I have to put thoppi first to avoid the
-- keyword @do@.  It would be nice if I could make the tracklang syntax
-- consistent, but maybe not a huge deal at the moment.
stroke_to_call :: Stroke -> Text
stroke_to_call s = case s of
    Thoppi t -> thoppi t
    Valantalai v -> pretty v
    Both t v -> pretty v <> thoppi t
    where
    thoppi t = case t of
        Thom -> "o"
        Tha -> "+"

k, t, n, d, u, i, o, p :: Note
k = Realize.Note (Valantalai Ki)
t = Realize.Note (Valantalai Ta)
n = Realize.Note (Valantalai Nam)
d = Realize.Note (Valantalai Din)
u = Realize.Note (Valantalai Chapu)
i = Realize.Note (Valantalai Dheem)
o = Realize.Note (Thoppi Thom)
p = Realize.Note (Thoppi Tha)

-- | @do@ would match score notation, but @do@ is a keyword.
-- Ultimately that's because score uses + for tha, and +o is an attr, while o+
-- is a bareword.  But perhaps I should change + to p in the score, and then
-- the left hand can go on the left side?
od :: Note
od = Realize.Note (Both Thom Din)

pk :: Note
pk = Realize.Note (Both Tha Ki)

both :: Thoppi -> Valantalai -> Note
both a b = Realize.Note  (Both a b)

(&) :: CallStack.Stack => Note -> Note -> Note
Realize.Note a & Realize.Note b = case (a, b) of
    (Thoppi a, Valantalai b) -> both a b
    (Valantalai b, Thoppi a) -> both a b
    _ -> errorStack $ "requires thoppi & valantalai: " <> showt (a, b)
a & b = errorStack $ "requires thoppi & valantalai: " <> showt (a, b)


-- * patterns

type Patterns = Realize.Patterns Stroke

__ :: Note
__ = Realize.Rest

defaults :: Patterns
defaults = S.check $ Realize.patterns
    [ (5, [k, t, k, n, o])
    , (6, [k, t, __, k, n, o])
    , (7, [k, __, t, __, k, n, o])
    , (8, [k, t, __, k, __, n, __, o])
    , (9, [k, __, t, __, k, __, n, __, o])
    ]

kt_kn_o :: Patterns
kt_kn_o = S.check $ Realize.patterns
    [ (5, [k, t, k, n, o])
    , (7, [k, t, __, k, n, __, o])
    , (9, [k, t, __, __, k, n, __, __, o])
    ]

families567 :: [Patterns]
families567 = map (S.check . Realize.patterns . zip [5..])
    [ [ [k, t, k, n, o]
      , [k, t, __, k, n, o]
      , [k, __, t, __, k, n, o]
      ]
    , [ [k, __, t, __, k, __, k, t, o, __]
      , [k, __, t, __, __, __, k, __, k, t, o, __]
      , [k, __, __, __, t, __, __, __, k, __, k, t, o, __]
      ]
    , [ [k, __, t, __, k, __, k, n, o, __]
      , [k, __, t, __, __, __, k, __, k, n, o, __]
      , [k, __, __, __, t, __, __, __, k, __, k, n, o, __]
      ]
    , [ [k, t, p, k, p, k, t, k, n, o]
      , kp <> [k, t, p, k, p, k, t, k, n, o]
      , kpnp <> [k, t, p, k, p, k, t, k, n, o]
      ]
    , [ [k, t, k, t, p, k, p, t, o, __]
      , [p, __, k, t, k, t, p, k, p, t, o, __]
      , [k, __, p, __, k, t, k, t, p, k, p, t, o, __]
      ]
    , [ [n, __, k, t, p, k, p, t, o, __]
      , [p, __, n, __, k, t, p, k, p, t, o, __]
      , [k, __, p, __, n, __, k, t, p, k, p, t, o, __]
      ]
    , [ [u, __, k, t, p, k, p, t, o, __]
      , [p, __, u, __, k, t, p, k, p, t, o, __]
      , [k, __, p, __, u, __, k, t, p, k, p, t, o, __]
      ]
    , [ [k, __, t, __, k, t, __, k, n, o]
      , kp <> [k, __, t, __, k, t, __, k, n, o]
      , kpnp <> [k, __, t, __, k, t, __, k, n, o]
      ]
    , [ [k, p, k, od, __, k, t, k, n, o]
      , [k, p, __, k, od, __, k, t, __, k, n, o]
      , [k, p, __, __, k, od, __, k, __, t, __, k, n, o]
      ]
    ]
    where
    kp = [k, p]
    kpnp = [k, p, n, p]
