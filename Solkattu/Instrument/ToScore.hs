-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to convert instrument-specific strokes to karya score.
module Solkattu.Instrument.ToScore where
import qualified Derive.Expr as Expr
import qualified Derive.ShowVal as ShowVal
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as Sequence
import qualified Solkattu.Solkattu as Solkattu

import Global


-- * ToScore

-- | Convert instrument-specific strokes into tracks.  This is a simple
-- intermediate data structure to bridge the solkattu types and the karya
-- types.
type ToScore stroke =
    [(Sequence.Duration, Realize.Note stroke)] -> ([Event], [(Text, [Event])])
    -- ^ (noteEvents, [(control, controlEvents)]).  A control named "*"
    -- becomes a pitch track.

type Event = (Sequence.Duration, Sequence.Duration, Text)


-- | A standard ToScore for simple percussion, with 0 duration and no control
-- tracks.
toScore :: Expr.ToExpr (Realize.Stroke stroke) => ToScore stroke
toScore strokes = (events, [])
    where
    events = do
        (start, dur, note) <- zip3 starts durs notes
        Just expr <- [toExpr note]
        let d = if Sequence.hasDuration note then dur else 0
        return (start, d, ShowVal.show_val expr)
    starts = scanl (+) 0 durs
    (durs, notes) = unzip strokes
    toExpr s = case s of
        Realize.Note stroke -> Just $ Expr.to_expr stroke
        Realize.Pattern p -> Just $ Expr.to_expr p
        Realize.Space Solkattu.Rest -> Nothing
        Realize.Space Solkattu.Sarva -> Nothing -- TODO
        Realize.Alignment {} -> Nothing
