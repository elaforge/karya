-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Repl cmds to deal with events.
module Cmd.Repl.LEvent where
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Regex as Regex
import qualified Util.Texts as Texts

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Instrument.Mridangam as Mridangam
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Expr as Expr
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Sel as Sel
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui

import           Global
import           Types


get :: Ui.M m => TrackId -> m Events.Events
get = fmap Track.track_events . Ui.get_track

events :: Ui.M m => TrackId -> m [Event.Event]
events = fmap Events.ascending . get

stretch :: ScoreTime -> Cmd.CmdL ()
stretch factor = do
    start <- Selection.start
    ModifyEvents.selection $ ModifyEvents.event $ stretch_event start factor

stretch_event :: ScoreTime -> ScoreTime -> Event.Event -> Event.Event
stretch_event start factor event
    | Event.start event < start = event
    | otherwise = Event.start_ %= (\p -> (p - start) * factor + start) $
        Event.duration_ %= (*factor) $ event

-- | Stretch events to fit in the given duration.
stretch_to :: TrackTime -> Cmd.CmdL ()
stretch_to dur = do
    selected <- Selection.events
    start <- Selection.start
    let maybe_end = Lists.maximum $
            mapMaybe (fmap Event.end . Lists.last . snd) selected
    whenJust maybe_end $ \end ->
        ModifyEvents.selection $ ModifyEvents.event $
            stretch_event start (dur / (end - start))

-- | Duration of the current selection, e.g. @stretch_to =<< sel_dur@.
sel_dur :: Cmd.M m => m TrackTime
sel_dur = do
    sel <- Selection.get
    return $ Sel.duration sel

modify_dur :: Cmd.M m => (ScoreTime -> ScoreTime) -> m ()
modify_dur = Edit.modify_dur

-- | Find all events containing the given substring.  Call with 'pp' to get
-- copy-pastable 's' codes.
find :: Text -> Cmd.CmdL [(Ui.Range, Text)]
find substr = find_f (substr `Text.isInfixOf`)

find_f :: (Text -> Bool) -> Cmd.CmdL [(Ui.Range, Text)]
find_f matches = fmap concat . concatMapM search =<< Ui.all_block_track_ids
    where
    search (block_id, track_ids) = forM track_ids $ \track_id -> do
        events <- Events.ascending . Track.track_events
            <$> Ui.get_track track_id
        let range e = Ui.Range (Just block_id) track_id
                (Event.start e) (Event.end e)
        return [(range event, Event.text event) |
            event <- events, matches (Event.text event)]

-- | Replace text on events.  Call with 'ModifyEvents.all_blocks' to replace it
-- everywhere, or 'ModifyEvents.note_tracks' for just note tracks.
replace :: Monad m => Text -> Text -> ModifyEvents.Track m
replace from to = ModifyEvents.text (Text.replace from to)

replace_exact :: Monad m => Text -> Text -> ModifyEvents.Track m
replace_exact from to = ModifyEvents.text $ \t -> if t == from then to else t

-- | Multiple replacements.  This is simultaneous replacement, so
-- [(a, b), (b, a)] will swap @a@ and @b@ as expected.
replace_many :: Monad m => [(Text, Text)] -> ModifyEvents.Track m
replace_many = ModifyEvents.text . Texts.replaceMany

replace_many_exact :: Monad m => [(Text, Text)] -> ModifyEvents.Track m
replace_many_exact repl = ModifyEvents.text $ \t -> fromMaybe t (lookup t repl)

-- | Upgrade thoppi on the right to thoppi on the left.  Delete when I don't
-- have any more scores like that.
upgrade_mridangam :: Cmd.M m => m ()
upgrade_mridangam = ModifyEvents.note_tracks $ replace_many_exact $
    Lists.keyOn (txt . List.reverse . untxt) $
        map call_of $
        Mridangam.make_both Mridangam.left_notes Mridangam.right_notes [] []
    where
    call_of (c, _, _) = Expr.unsym c

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
    where reg = Regex.compileUnsafe regex

-- * quantize

-- | Which end of the event to quantize.
data Mode = Start | End | Both

quantize_sel :: Cmd.M m => Text -> m ()
quantize_sel = ModifyEvents.selection . quantize_timestep Both

-- | Quantize to a TimeStep's duration.  What this does is snap the edges of
-- the event to the nearest timestep.
quantize_timestep :: Ui.M m => Mode -> Text -> ModifyEvents.Track m
quantize_timestep mode step block_id track_id events = do
    step <- Ui.require_right ("parsing timestep: "<>) $
        TimeStep.parse_time_step step
    tracknum <- Ui.get_tracknum_of block_id track_id
    points <- TimeStep.get_points_from TimeStep.Advance block_id tracknum 0 step
    return $ Just $ resolve_conflicts points $
        snd $ List.mapAccumL (quantize_event mode) points events

-- | Quantize can put two events in the same spot.  Resolve the conflicts by
-- bumping events back until they don't conflict.  If I run out of timesteps,
-- bump by 1.
resolve_conflicts :: [TrackTime] -> [Event.Event] -> [Event.Event]
resolve_conflicts _ [] = []
resolve_conflicts points (event : events) =
    event : resolve_conflicts points_after
        (map (Event.start_ #= bump) group ++ rest)
    where
    (group, rest) = span ((== Event.start event) . Event.start) events
    bump = fromMaybe (Event.start event + 1) (Lists.head points_after)
    points_after = dropWhile (<= Event.start event) points

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
    quantize_start = Event.start_ %= quantize start_points
    quantize_end event = Event.end_ %= quantize (end_points event) $ event
    zero = Event.duration event == 0
    start_points = Lists.dropBefore id (Event.start event) points_
    end_points e = dropWhile (<= Event.start e) $
        Lists.dropBefore id (Event.end e) start_points

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
    Ui.insert_events track_id
        [Event.event (start + pos) dur text | (start, dur, text) <- events]
