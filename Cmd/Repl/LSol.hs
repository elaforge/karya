-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for solkattu.  This re-exports "Derive.Solkattu.Score" so I can
-- write korvais there and directly insert them into the score from here.
module Cmd.Repl.LSol (
    module Cmd.Repl.LSol
    , module Derive.Solkattu.Score
) where
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Mridangam as Mridangam
import qualified Derive.Solkattu.Realize as Realize
import Derive.Solkattu.Score
import qualified Derive.Solkattu.Solkattu as Solkattu

import Types


-- | Insert the korvai at the selection.
insert :: Cmd.M m => Bool -> TrackTime -> Korvai.Korvai -> m ()
insert realize_patterns akshara_dur korvai = do
    let stroke_dur = akshara_dur
            / fromIntegral (Solkattu.tala_nadai (Korvai.korvai_tala korvai))
    (_, _, track_id, at) <- Selection.get_insert
    events <- map (Event.move (+at)) . Events.ascending <$>
        realize_korvai realize_patterns stroke_dur korvai
    State.remove_events track_id (map Event.start events)
    State.insert_events track_id events

realize_korvai :: State.M m => Bool -> TrackTime -> Korvai.Korvai
    -> m Events.Events
realize_korvai realize_patterns stroke_dur korvai = do
    strokes <- State.require_right id $ Korvai.realize realize_patterns korvai
    return $ Events.from_list
        [ Event.event start 0 (Mridangam.stroke_to_call stroke)
        | (start, Realize.Note stroke)
            <- zip (Seq.range_ 0 stroke_dur) strokes
        ]
