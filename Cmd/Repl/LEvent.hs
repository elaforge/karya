-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Repl cmds to deal with events.
module Cmd.Repl.LEvent where
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.Regex as Regex
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Sel as Sel
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import Global
import Types


get :: State.M m => TrackId -> m Events.Events
get = fmap Track.track_events . State.get_track

stretch :: ScoreTime -> Cmd.CmdL ()
stretch n = do
    selected <- Selection.events
    let maybe_start = Seq.minimum $
            mapMaybe (\(_, _, events) -> Event.start <$> Seq.head events)
                selected
    whenJust maybe_start $ \start ->
        ModifyEvents.selection $ ModifyEvents.event $ stretch_event start n

stretch_event :: ScoreTime -> ScoreTime -> Event.Event -> Event.Event
stretch_event start n =
    Event.move (\p -> (p - start) * n + start) . Event.modify_duration (*n)

-- | Stretch events to fit in the given duration.
stretch_to :: TrackTime -> Cmd.CmdL ()
stretch_to dur = do
    selected <- Selection.events
    let maybe_end = Seq.maximum $
            mapMaybe (\(_, _, events) -> Event.end <$> Seq.last events) selected
        maybe_start = Seq.minimum [s | (_, (s, _), _) <- selected]
    whenJust ((,) <$> maybe_start <*> maybe_end) $ \(start, end) ->
        ModifyEvents.selection $ ModifyEvents.event $
            stretch_event start (dur / (end - start))

-- | Duration of the current selection, e.g. @stretch_to =<< sel_dur@.
sel_dur :: Cmd.M m => m TrackTime
sel_dur = do
    (_, sel) <- Selection.get
    return $ Sel.duration sel

modify_dur :: Cmd.M m => (ScoreTime -> ScoreTime) -> m ()
modify_dur = Edit.modify_dur

-- | Find all events containing the given substring.  Call with 'pp' to get
-- copy-pastable 's' codes.
find :: Text -> Cmd.CmdL [(State.Range, Text)]
find substr = find_f (substr `Text.isInfixOf`)

find_f :: (Text -> Bool) -> Cmd.CmdL [(State.Range, Text)]
find_f matches = fmap concat . concatMapM search =<< State.all_block_track_ids
    where
    search (block_id, track_ids) = forM track_ids $ \track_id -> do
        events <- Events.ascending . Track.track_events
            <$> State.get_track track_id
        let range e = State.Range (Just block_id) track_id
                (Event.start e) (Event.end e)
        return [(range event, Event.event_text event) |
            event <- events, matches (Event.event_text event)]

-- | Replace text on events.  Call with 'ModifyEvents.all_blocks' to replace it
-- everywhere, or 'ModifyEvents.all_note_tracks' for just note tracks.
replace :: Monad m => Text -> Text -> ModifyEvents.Track m
replace from to = ModifyEvents.text (Text.replace from to)

-- | Multiple replacements.  This is simultaneous replacement, so
-- [(a, b), (b, a)] will swap @a@ and @b@ as expected.
replace_many :: Monad m => [(Text, Text)] -> ModifyEvents.Track m
replace_many = ModifyEvents.text . TextUtil.replaceMany

-- | Modify event text with 'ModifyEvents.substitute'.
--
-- For example, to turn \"si .5 .3\" into \".3 | i .5\":
--
-- > ModifyEvents.control_tracks $
-- >    LEvent.replace_pattern ("si"<>w<>w) [F 1, "| i", F 0]
--
-- 'w' is a word, 'ws' is >=0 words, 'ws1' is >=1 words, and a string matches
-- literal text.
replace_pattern :: Cmd.M m => ModifyEvents.Parser
    -> [ModifyEvents.Replacement] -> ModifyEvents.Track m
replace_pattern from to =
    ModifyEvents.failable_text (ModifyEvents.substitute from to)

-- | Modify event text with 'Regex.substituteGroups'.
--
-- For example, to turn \"si .5 .3\" into \".3 | i .5\":
--
-- > ModifyEvents.control_tracks $ LEvent.replace_regex
-- >    "si (\\W+) (\\W+)" (\[f0 f1] -> f1 <> " | i " <> f0)
replace_regex :: Monad m => String -> ([Text] -> Text) -> ModifyEvents.Track m
replace_regex regex modify =
    ModifyEvents.text (Regex.substituteGroups reg (const modify))
    where reg = Regex.compileUnsafe "LEvent.replace_regex" regex

-- * quantize

-- | Which end of the event to quantize.
data Mode = Start | End | Both

quantize_sel :: Cmd.M m => Text -> m ()
quantize_sel = ModifyEvents.selection . quantize_timestep Both

-- | Quantize to a TimeStep's duration.  What this does is snap the edges of
-- the event to the nearest timestep.
quantize_timestep :: State.M m => Mode -> Text -> ModifyEvents.Track m
quantize_timestep mode step block_id track_id events = do
    step <- State.require_right ("parsing timestep: "<>) $
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

-- * negative events

invert :: Monad m => ModifyEvents.Track m
invert = ModifyEvents.event $ \event -> event
    { Event.start = Event.end event
    , Event.duration = - Event.duration event
    }
