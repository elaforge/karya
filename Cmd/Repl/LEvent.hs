{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Repl cmds to deal with events.
module Cmd.Repl.LEvent where
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.ScoreTime as ScoreTime
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
    ModifyEvents.selection $ ModifyEvents.event $
        Event.move (\p -> (p - start) * n + start) . Event.modify_duration (*n)

modify_dur :: (Cmd.M m) => (ScoreTime -> ScoreTime) -> m ()
modify_dur = Edit.modify_dur

-- | Find all events having the given substring.  Call with 'pp' to get
-- copy-pastable 's' codes.
find :: Text -> Cmd.CmdL [(State.Range, Text)]
find text = fmap concat . concatMapM search =<< State.all_block_track_ids
    where
    bs = Event.from_text text
    search (block_id, track_ids) = forM track_ids $ \track_id -> do
        events <- Events.ascending . Track.track_events
            <$> State.get_track track_id
        let range e = State.Range (Just block_id) track_id
                (Event.start e) (Event.end e)
        return [(range event, Event.event_text event) | event <- events,
            bs `Char8.isInfixOf` Event.event_bytestring event]

-- | Replace text on events.  Call with 'ModifyEvents.all_blocks' to replace it
-- everywhere, or 'ModifyEvents.all_note_tracks' for just note tracks.
replace :: (Monad m) => Text -> Text -> ModifyEvents.Track m
replace from to = ModifyEvents.text (Text.replace from to)

-- * quantize

data Mode = Start | End | Both

quantize_sel :: (Cmd.M m) => Text -> m ()
quantize_sel = ModifyEvents.selection . quantize_timestep Both

-- | Quantize to a TimeStep's duration.  Actually it just takes the timestep at
-- time 0, so it won't be correct if the timestep changes.
quantize_timestep :: (Cmd.M m) => Mode -> Text -> ModifyEvents.Track m
quantize_timestep mode step block_id track_id events = do
    step <- Cmd.require_right ("parsing timestep: "++) $
        TimeStep.parse_time_step step
    tracknum <- State.get_tracknum_of block_id track_id
    dur <- Cmd.require_msg ("can't step: " ++ TimeStep.show_time_step step)
        =<< TimeStep.advance step block_id tracknum 0
    return $ Just $ map (quantize_event mode dur) events

quantize_event :: Mode -> ScoreTime -> Event.Event -> Event.Event
quantize_event mode dur event = case mode of
    Start -> Event.move (quantize dur) event
    End -> Event.modify_end (quantize dur) event
    Both -> quantize_event End dur $ quantize_event Start dur event

quantize :: ScoreTime -> ScoreTime -> ScoreTime
quantize dur time = ScoreTime.double (fromIntegral int) * dur
    where
    int :: Integer
    int = round (time / dur)
