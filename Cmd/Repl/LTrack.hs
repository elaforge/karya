-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds for track level operations.
module Cmd.Repl.LTrack where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection

import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ScoreT as ScoreT
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui

import           Global
import           Types


-- | List all tracks, along with the number of blocks each one appears in.
list :: Cmd.CmdL [(TrackId, Int)]
list = do
    trackIds <- Ui.allTrackIds
    counts <- map length <$> mapM Ui.blocksWithTrackId trackIds
    pure $ zip trackIds counts

gc :: Cmd.CmdL [TrackId]
gc = do
    tids <- orphans
    mapM_ Ui.destroyTrack tids
    pure tids

-- | Tracks that don't appear in any block.
orphans :: Cmd.CmdL [TrackId]
orphans = Set.elems <$> Create.orphanTracks

-- | Remove tracks with no events from the given block.
removeEmpty :: BlockId -> Cmd.CmdL ()
removeEmpty blockId = do
    trackIds <- Block.blockTrackIds <$> Ui.getBlock blockId
    mapM_ Ui.destroyTrack
        =<< filterM (fmap Events.null . Ui.getEvents) trackIds

removeAllEmpty :: Cmd.CmdL ()
removeAllEmpty = mapM_ removeEmpty =<< Ui.allBlockIds

mapWidths :: (Text -> Bool) -> (Types.Width -> Types.Width) -> Cmd.CmdL ()
mapWidths wanted f = do
    blockIds <- Ui.allBlockIds
    forM_ blockIds $ \blockId -> do
        tracknums <- map Ui.trackTracknum
            . filter (wanted . Ui.trackTitle) <$>
                TrackTree.tracksOf blockId
        widths <- map Block.trackWidth <$>
            mapM (Ui.getBlockTrackAt blockId) tracknums
        zipWithM_ (Ui.setTrackWidth blockId)
            tracknums (map f widths)

-- | Transform all track titles.
mapTitles :: (Text -> Text) -> Cmd.CmdL ()
mapTitles f = do
    tids <- Ui.allTrackIds
    mapM_ (flip Ui.modifyTrackTitle f) tids

renameInstruments :: [(Text, Text)] -> Cmd.CmdL ()
renameInstruments renames = mapTitles $ \t -> fromMaybe t $ lookup t renames

replace :: Text -> Text -> Cmd.CmdL ()
replace from to = mapTitles $ Text.replace from to

-- | Find all tracks with the given string in their title.  You can use
-- 'Ui.blocks_with_track_id' to find the blocks with the tracks, and
-- 'map_titles' or 'replace' to change the titles.
find :: Cmd.M m => Text -> m [(TrackId, Text)]
find search = findF (search `Text.isInfixOf`)

findF :: Cmd.M m => (Text -> Bool) -> m [(TrackId, Text)]
findF matches = do
    tids <- Ui.allTrackIds
    titles <- mapM Ui.getTrackTitle tids
    pure [(tid, title) | (tid, title) <- zip tids titles, matches title]

-- * manipulation

-- | Duplicate a track from one block to another.  The underlying track is
-- the same, so edits in one of its occurrances will be reflected in all of its
-- blocks.
duplicate :: Ui.M m => BlockId -> TrackNum -> BlockId -> TrackNum -> m ()
duplicate sourceBlock sourceTracknum destBlock destTracknum = do
    track <- Ui.getBlockTrackAt sourceBlock sourceTracknum
    Ui.insertTrack destBlock destTracknum track

-- * events

events :: Ui.M m => TrackId -> m [Event.Event]
events = fmap Events.ascending . Ui.getEvents

selected :: Cmd.M m => m [Event.Event]
selected = do
    (_, _, trackIds, range) <- Selection.tracks
    trackId <- Cmd.require "selected track" (Lists.head trackIds)
    Events.ascending . Events.inRange range <$> Ui.getEvents trackId

eventsRange :: TrackId -> ScoreTime -> ScoreTime -> Cmd.CmdL [Event.Event]
eventsRange trackId start end =
    Events.ascending . Events.inRange (Events.Range start end) <$>
        Ui.getEvents trackId

selectedNotation :: Cmd.M m => TrackTime -> m Text
selectedNotation step = do
    events <- selected
    (start, end) <- Events.rangeTimes <$> Selection.range
    pure $ toNotation start step end events

-- | Reduce event text to notation at a fixed time increment.  It only works
-- out if each event only has a single letter.
toNotation :: TrackTime -> TrackTime -> TrackTime -> [Event.Event] -> Text
toNotation start step end = mconcat . go (Lists.range' start end step)
    where
    go _ [] = []
    go [] ts = replicate (length ts) " "
    go (t:ts) (e:es)
        | Event.start e > t = " " : go ts (e:es)
        | Event.start e == t = Event.text e : go ts es
        | otherwise = " " : go (t:ts) es

-- | 4 measures per line, 16 time steps per measure.
formatMeasures :: String -> [String]
formatMeasures =
    map (List.intercalate "|") . Lists.chunked 4 . Lists.chunked 16


-- * strip controls

dropDups :: Cmd.CmdL ()
dropDups = ModifyEvents.selection $ ModifyEvents.events $
    pure . Lists.dropDups Event.text

-- * waveform

waveform :: Cmd.CmdT IO ()
waveform = do
    mapM_ toggleWaveform =<< Selection.trackIds
    -- Invalidate to force it to add or remove waveforms.  It's a big hammer
    -- but it works and it should be uncommon to toggle this.
    Cmd.invalidatePerformances

toggleWaveform :: TrackId -> Cmd.CmdT IO ()
toggleWaveform trackId = Ui.modifyWaveform trackId not


-- * signal render

filled :: Cmd.CmdL ()
filled = mapM_ (Ui.setRenderStyle (Track.Filled Nothing))
    =<< Selection.trackIds

line :: Cmd.CmdL ()
line = mapM_ (Ui.setRenderStyle (Track.Line Nothing))
    =<< Selection.trackIds

notePitch :: Cmd.CmdL ()
notePitch = nline "#"

-- | Pass \"#\" for the pitch track.
nline :: Text -> Cmd.CmdL ()
nline = noteRender Track.Line

nfilled :: Text -> Cmd.CmdL ()
nfilled = noteRender Track.Filled

noteRender :: Cmd.M m => (Maybe Track.RenderSource -> Track.RenderStyle)
    -> Text -- ^ Either a control name, or a #-prefixed pitch name.
    -> m ()
noteRender mode controlName = do
    control <- Cmd.requireRight id $ ScoreT.parseGenericControl controlName
    trackIds <- Selection.trackIds
    trackIds <- filterM isNote trackIds
    mapM_ (Ui.setRenderStyle
        (mode (Just (either Track.Control Track.Pitch control)))) trackIds
    where
    isNote = fmap ParseTitle.isNoteTrack . Ui.getTrackTitle

noRender :: Cmd.CmdL ()
noRender = mapM_ (Ui.setRenderStyle Track.NoRender) =<< Selection.trackIds
