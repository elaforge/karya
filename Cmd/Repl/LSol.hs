-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for solkattu.  This re-exports "Derive.Solkattu.Score" so I can
-- write korvais there and directly insert them into the score from here.
module Cmd.Repl.LSol (
    module Cmd.Repl.LSol
    , module Derive.Solkattu.Score
) where
import qualified Data.Text as Text

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ui as Ui

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Derive.Expr as Expr
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Realize as Realize
import Derive.Solkattu.Score
import qualified Derive.Solkattu.Sequence as Sequence

import Global
import Types


insert_m :: Cmd.M m => Bool -> TrackTime -> Korvai.Korvai -> Int -> m ()
insert_m = insert Korvai.mridangam

insert_k1 :: Cmd.M m => Bool -> TrackTime -> Korvai.Korvai -> Int -> m ()
insert_k1 = insert Korvai.kendang_tunggal

insert_r :: Cmd.M m => Bool -> TrackTime -> Korvai.Korvai -> Int -> m ()
insert_r = insert Korvai.reyong

-- | Insert the korvai at the selection.
insert :: (Expr.ToExpr (Realize.Stroke stroke), Pretty stroke, Cmd.M m) =>
    Korvai.GetInstrument stroke -> Bool -> TrackTime -> Korvai.Korvai
    -> Int -> m ()
insert instrument realize_patterns akshara_dur korvai index = do
    (_, _, track_id, at) <- Selection.get_insert
    let place = (Event.start_ %= ((+at) . (*akshara_dur)))
            . (Event.duration_ %= (*akshara_dur))
    events <- map place . Events.ascending
        <$> realize_korvai instrument realize_patterns korvai index
    Ui.remove_events track_id events
    Ui.insert_events track_id events

realize_korvai ::
    (Expr.ToExpr (Realize.Stroke stroke), Pretty stroke, Ui.M m) =>
    Korvai.GetInstrument stroke -> Bool -> Korvai.Korvai -> Int
    -> m Events.Events
realize_korvai instrument realize_patterns korvai index = do
    (strokes, warning) <- Ui.require_right id $
        Korvai.realize instrument realize_patterns korvai
            !! index
    unless (Text.null warning) $ Ui.throw warning
    return $ Events.from_list $ strokes_to_events strokes

strokes_to_events :: Expr.ToExpr (Realize.Stroke a) =>
    [(Sequence.Tempo, Realize.Note a)] -> [Event.Event]
strokes_to_events strokes =
    [ Event.event (realToFrac start) (if has_dur then realToFrac dur else 0)
        (ShowVal.show_val expr)
    | (start, dur, Just (expr, has_dur)) <- zip3 starts durs (map to_expr notes)
    ]
    where
    starts = scanl (+) 0 durs
    (durs, notes) = unzip $ Realize.tempo_to_duration strokes
    to_expr s = case s of
        Realize.Note stroke -> Just (Expr.to_expr stroke, False)
        Realize.Pattern p -> Just (Expr.to_expr p, True)
        Realize.Rest -> Nothing
