-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Repl cmds to deal with events.
module Cmd.Repl.LEvent where
import qualified Data.List as List
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import Types


stretch :: ScoreTime -> Cmd.CmdL ()
stretch n = do
    selected <- Selection.events
    let start = fromMaybe 0 $ Seq.minimum $
            map (\(_, _, evts) -> maybe 0 Event.start (Seq.head evts)) selected
    ModifyEvents.selection $ ModifyEvents.event $ stretch_event start n

stretch_event :: ScoreTime -> ScoreTime -> Event.Event -> Event.Event
stretch_event start n =
    Event.move (\p -> (p - start) * n + start) . Event.modify_duration (*n)

modify_dur :: Cmd.M m => (ScoreTime -> ScoreTime) -> m ()
modify_dur = Edit.modify_dur

-- | Find all events having the given substring.  Call with 'pp' to get
-- copy-pastable 's' codes.
find :: Text -> Cmd.CmdL [(State.Range, Text)]
find text = fmap concat . concatMapM search =<< State.all_block_track_ids
    where
    search (block_id, track_ids) = forM track_ids $ \track_id -> do
        events <- Events.ascending . Track.track_events
            <$> State.get_track track_id
        let range e = State.Range (Just block_id) track_id
                (Event.start e) (Event.end e)
        return [(range event, Event.event_text event) | event <- events,
            text `Text.isInfixOf` Event.event_text event]

-- | Replace text on events.  Call with 'ModifyEvents.all_blocks' to replace it
-- everywhere, or 'ModifyEvents.all_note_tracks' for just note tracks.
replace :: Monad m => Text -> Text -> ModifyEvents.Track m
replace from to = ModifyEvents.text (Text.replace from to)

replace_many :: Monad m => [(Text, Text)] -> ModifyEvents.Track m
replace_many = ModifyEvents.text . TextUtil.replaceMany

-- * quantize

-- | Which end of the event to quantize.
data Mode = Start | End | Both

quantize_sel :: Cmd.M m => Text -> m ()
quantize_sel = ModifyEvents.selection . quantize_timestep Both

-- | Quantize to a TimeStep's duration.  What this does is snap the edges of
-- the event to the nearest timestep.
quantize_timestep :: State.M m => Mode -> Text -> ModifyEvents.Track m
quantize_timestep mode step block_id track_id events = do
    step <- Cmd.require_right ("parsing timestep: "++) $
        TimeStep.parse_time_step step
    tracknum <- State.get_tracknum_of block_id track_id
    points <- TimeStep.get_points_from TimeStep.Advance block_id tracknum 0 step
    return $ Just $ snd $ List.mapAccumL (quantize_event mode) points events

-- | Zero-duration events will remain zero duration, and not be affected by
-- End quantization.  Non-zero-duration events will never be quantized to zero
-- duration.
quantize_event :: Mode -> [TrackTime] -> Event.Event
    -> ([TrackTime], Event.Event)
quantize_event mode points_ event = (start_points, quantize_event event)
    where
    quantize_event = case mode of
        Start -> quantize_start
        End
            | zero -> id
            | otherwise -> quantize_end
        Both
            | zero -> quantize_start
            | otherwise -> quantize_end . quantize_start
    quantize_start = Event.move (quantize start_points)
    quantize_end event =
        Event.modify_end (quantize (end_points (Event.end event))) event
    zero = Event.duration event == 0
    start_points = TimeStep.drop_before (Event.start event) points_
    end_points end = dropWhile (< end) start_points

quantize :: [TrackTime] -> TrackTime -> TrackTime
quantize points t = case points of
    t1 : t2 : _
        | abs (t - t1) <= abs (t2 - t) -> t1
        | otherwise -> t2
    [t1] -> t1
    [] -> t

-- * insert

-- | Insert an event directly.
insert :: Cmd.M m => [(ScoreTime, ScoreTime, Text)] -> m ()
insert events = do
    (_, _, track_id, pos) <- Selection.get_insert
    State.insert_events track_id
        [Event.event (start + pos) dur text | (start, dur, text) <- events]
