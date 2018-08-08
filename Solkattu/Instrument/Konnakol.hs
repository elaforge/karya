-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Realize to konnakol.  This is simpler than instruments since I just use
-- the sollus directly, but I still need realizations for 'Patterns'.
module Solkattu.Instrument.Konnakol where
import qualified Derive.Expr as Expr
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import Solkattu.Solkattu (Sollu(..))

import Global


note :: Sollu -> Realize.SNote Sollu
note NoSollu = Realize.rest
note sollu = S.Note (Realize.Note (Realize.stroke sollu))

defaultPatterns :: Realize.Patterns Sollu
defaultPatterns = Solkattu.check $ patterns
    [ (5, [Ta, Din, Gin, Na, Thom])
    , (6, [Ta, Din, __, Gin, Na, Thom])
    , (7, [Ta, __, Din, __, Gin, Na, Thom])
    , (8, [Ta, Din, __, Gin, __, Na, __, Thom])
    , (9, [Ta, __, Din, __, Gin, __, Na, __, Thom])
    ]

alternatePatterns :: Realize.Patterns Sollu
alternatePatterns = Solkattu.check $ patterns
    [ (5, [Ta, Di, Ki, Tha, Thom])
    , (6, [Ta, Di, __, Ki, Tha, Thom])
    , (7, [Ta, __, Di, __, Ki, Tha, Thom])
    , (8, [Ta, Di, __, Ki, __, Tha, __, Thom])
    , (9, [Ta, __, Di, __, Ki, __, Tha, __, Thom])
    ]

__ :: Sollu
__ = NoSollu

patterns :: [(S.Matra, [Sollu])] -> Either Text (Realize.Patterns Sollu)
patterns = Realize.patterns . map (second (map note)) . (defaultNakatiku++)
    . map (first Solkattu.pattern)

defaultNakatiku :: [(Solkattu.Pattern, [Sollu])]
defaultNakatiku =
    [ (Solkattu.nakatiku, [Na, Ka, Ti, Ku, Ta, Ri, Ki, Ta])
    ]

instance Expr.ToExpr Sollu where
    to_expr = Expr.generator0 . Expr.Symbol . pretty
instance Expr.ToExpr (Realize.Stroke Sollu) where
    to_expr = Realize.toExpr
