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
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Sel as Sel
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui

import           Global
import           Types


get :: Ui.M m => TrackId -> m Events.Events
get = fmap Track.trackEvents . Ui.getTrack

events :: Ui.M m => TrackId -> m [Event.Event]
events = fmap Events.ascending . get

stretch :: ScoreTime -> Cmd.CmdL ()
stretch factor = do
    start <- Selection.start
    ModifyEvents.selection $ ModifyEvents.event $ stretchEvent start factor

stretchEvent :: ScoreTime -> ScoreTime -> Event.Event -> Event.Event
stretchEvent start factor event
    | Event.start event < start = event
    | otherwise = Event.start_ %= (\p -> (p - start) * factor + start) $
        Event.duration_ %= (*factor) $ event

-- | Stretch events to fit in the given duration.
stretchTo :: TrackTime -> Cmd.CmdL ()
stretchTo dur = do
    selected <- Selection.events
    start <- Selection.start
    let maybeEnd = Lists.maximum $
            mapMaybe (fmap Event.end . Lists.last . snd) selected
    whenJust maybeEnd $ \end ->
        ModifyEvents.selection $ ModifyEvents.event $
            stretchEvent start (dur / (end - start))

-- | Duration of the current selection, e.g. @stretch_to =<< sel_dur@.
selDur :: Cmd.M m => m TrackTime
selDur = do
    sel <- Selection.get
    pure $ Sel.duration sel

modifyDur :: Cmd.M m => (ScoreTime -> ScoreTime) -> m ()
modifyDur = Edit.modifyDur

-- | Find all events containing the given substring.  Call with 'pp' to get
-- copy-pastable 's' codes.
find :: Text -> Cmd.CmdL [(Ui.Range, Text)]
find substr = findF (substr `Text.isInfixOf`)

findF :: (Text -> Bool) -> Cmd.CmdL [(Ui.Range, Text)]
findF matches = fmap concat . concatMapM search =<< Ui.allBlockTrackIds
    where
    search (blockId, trackIds) = forM trackIds $ \trackId -> do
        events <- Events.ascending . Track.trackEvents
            <$> Ui.getTrack trackId
        let range e = Ui.Range (Just blockId) trackId
                (Event.start e) (Event.end e)
        pure [(range event, Event.text event) |
            event <- events, matches (Event.text event)]

-- | Replace text on events.  Call with 'ModifyEvents.all_blocks' to replace it
-- everywhere, or 'ModifyEvents.note_tracks' for just note tracks.
replace :: Monad m => Text -> Text -> ModifyEvents.Track m
replace from to = ModifyEvents.text (Text.replace from to)

replaceExact :: Monad m => Text -> Text -> ModifyEvents.Track m
replaceExact from to = ModifyEvents.text $ \t -> if t == from then to else t

-- | Multiple replacements.  This is simultaneous replacement, so
-- [(a, b), (b, a)] will swap @a@ and @b@ as expected.
replaceMany :: Monad m => [(Text, Text)] -> ModifyEvents.Track m
replaceMany = ModifyEvents.text . Texts.replaceMany

replaceManyExact :: Monad m => [(Text, Text)] -> ModifyEvents.Track m
replaceManyExact repl = ModifyEvents.text $ \t -> fromMaybe t (lookup t repl)

-- -- | Upgrade thoppi on the right to thoppi on the left.  Delete when I don't
-- -- have any more scores like that.
-- upgrade_mridangam :: Cmd.M m => m ()
-- upgrade_mridangam = ModifyEvents.note_tracks $ replace_many_exact $
--     Lists.keyOn (txt . List.reverse . untxt) $
--         map call_of $
--         Mridangam.make_both Mridangam.left_notes Mridangam.right_notes [] []
--     where
--     call_of (c, _, _) = Expr.unsym c

-- | Modify event text with 'ModifyEvents.substitute'.
--
-- For example, to turn \"si .5 .3\" into \".3 | i .5\":
--
-- > ModifyEvents.control_tracks $
-- >    LEvent.replace_pattern ("si"<>w<>w) [F 1, "| i", F 0]
--
-- 'w' is a word, 'ws' is >=0 words, 'ws1' is >=1 words, and a string matches
-- literal text.
replacePattern :: Cmd.M m => ModifyEvents.Parser
    -> [ModifyEvents.Replacement] -> ModifyEvents.Track m
replacePattern from to =
    ModifyEvents.failableText (ModifyEvents.substitute from to)

-- | Modify event text with 'Regex.substituteGroups'.
--
-- For example, to turn \"si .5 .3\" into \".3 | i .5\":
--
-- > ModifyEvents.control_tracks $ LEvent.replace_regex
-- >    "si (\\W+) (\\W+)" (\[f0 f1] -> f1 <> " | i " <> f0)
replaceRegex :: Monad m => String -> ([Text] -> Text) -> ModifyEvents.Track m
replaceRegex regex modify =
    ModifyEvents.text (Regex.substituteGroups reg (const modify))
    where reg = Regex.compileUnsafe regex

-- * quantize

-- | Which end of the event to quantize.
data Mode = Start | End | Both

quantizeSel :: Cmd.M m => Text -> m ()
quantizeSel = ModifyEvents.selection . quantizeTimestep Both

-- | Quantize to a TimeStep's duration.  What this does is snap the edges of
-- the event to the nearest timestep.
quantizeTimestep :: Ui.M m => Mode -> Text -> ModifyEvents.Track m
quantizeTimestep mode step blockId trackId events = do
    step <- Ui.requireRight ("parsing timestep: "<>) $
        TimeStep.parseTimeStep step
    tracknum <- Ui.getTracknumOf blockId trackId
    points <- TimeStep.getPointsFrom TimeStep.Advance blockId tracknum 0 step
    pure $ Just $ resolveConflicts points $
        snd $ List.mapAccumL (quantizeEvent mode) points events

-- | Quantize can put two events in the same spot.  Resolve the conflicts by
-- bumping events back until they don't conflict.  If I run out of timesteps,
-- bump by 1.
resolveConflicts :: [TrackTime] -> [Event.Event] -> [Event.Event]
resolveConflicts _ [] = []
resolveConflicts points (event : events) =
    event : resolveConflicts pointsAfter
        (map (Event.start_ #= bump) group ++ rest)
    where
    (group, rest) = span ((== Event.start event) . Event.start) events
    bump = fromMaybe (Event.start event + 1) (Lists.head pointsAfter)
    pointsAfter = dropWhile (<= Event.start event) points

-- | Zero-duration events will remain zero duration, and not be affected by
-- End quantization.  Non-zero-duration events will never be quantized to zero
-- duration.
quantizeEvent :: Mode -> [TrackTime] -> Event.Event
    -> ([TrackTime], Event.Event)
quantizeEvent mode points_ event = (startPoints, quantizeEvent event)
    where
    quantizeEvent = case mode of
        Start -> quantizeStart
        End
            | zero -> id
            | otherwise -> quantizeEnd
        Both
            | zero -> quantizeStart
            | otherwise -> quantizeEnd . quantizeStart
    quantizeStart = Event.start_ %= quantize startPoints
    quantizeEnd event = Event.end_ %= quantize (endPoints event) $ event
    zero = Event.duration event == 0
    startPoints = Lists.dropBefore id (Event.start event) points_
    endPoints e = dropWhile (<= Event.start e) $
        Lists.dropBefore id (Event.end e) startPoints

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
    (_, _, trackId, pos) <- Selection.getInsert
    Ui.insertEvents trackId
        [Event.event (start + pos) dur text | (start, dur, text) <- events]
