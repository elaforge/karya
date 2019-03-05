-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds for track level operations.
module Cmd.Repl.LTrack where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Seq as Seq
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
    track_ids <- Ui.all_track_ids
    counts <- map length <$> mapM Ui.blocks_with_track_id track_ids
    return $ zip track_ids counts

gc :: Cmd.CmdL [TrackId]
gc = do
    tids <- orphans
    mapM_ Ui.destroy_track tids
    return tids

-- | Tracks that don't appear in any block.
orphans :: Cmd.CmdL [TrackId]
orphans = Set.elems <$> Create.orphan_tracks

-- | Remove tracks with no events from the given block.
remove_empty :: BlockId -> Cmd.CmdL ()
remove_empty block_id = do
    track_ids <- Block.block_track_ids <$> Ui.get_block block_id
    mapM_ Ui.destroy_track
        =<< filterM (fmap Events.null . Ui.get_events) track_ids

remove_all_empty :: Cmd.CmdL ()
remove_all_empty = mapM_ remove_empty =<< Ui.all_block_ids

map_widths :: (Text -> Bool) -> (Types.Width -> Types.Width) -> Cmd.CmdL ()
map_widths wanted f = do
    block_ids <- Ui.all_block_ids
    forM_ block_ids $ \block_id -> do
        tracknums <- map Ui.track_tracknum
            . filter (wanted . Ui.track_title) <$>
                TrackTree.tracks_of block_id
        widths <- map Block.track_width <$>
            mapM (Ui.get_block_track_at block_id) tracknums
        zipWithM_ (Ui.set_track_width block_id)
            tracknums (map f widths)

-- | Transform all track titles.
map_titles :: (Text -> Text) -> Cmd.CmdL ()
map_titles f = do
    tids <- Ui.all_track_ids
    mapM_ (flip Ui.modify_track_title f) tids

rename_instruments :: [(Text, Text)] -> Cmd.CmdL ()
rename_instruments renames = map_titles $ \t -> fromMaybe t $ lookup t renames

replace :: Text -> Text -> Cmd.CmdL ()
replace from to = map_titles $ Text.replace from to

-- | Find all tracks with the given string in their title.  You can use
-- 'Ui.blocks_with_track_id' to find the blocks with the tracks, and
-- 'map_titles' or 'replace' to change the titles.
find :: Cmd.M m => Text -> m [(TrackId, Text)]
find search = find_f (search `Text.isInfixOf`)

find_f :: Cmd.M m => (Text -> Bool) -> m [(TrackId, Text)]
find_f matches = do
    tids <- Ui.all_track_ids
    titles <- mapM Ui.get_track_title tids
    return [(tid, title) | (tid, title) <- zip tids titles, matches title]

-- * manipulation

-- | Duplicate a track from one block to another.  The underlying track is
-- the same, so edits in one of its occurrances will be reflected in all of its
-- blocks.
duplicate :: Ui.M m => BlockId -> TrackNum -> BlockId -> TrackNum -> m ()
duplicate source_block source_tracknum dest_block dest_tracknum = do
    track <- Ui.get_block_track_at source_block source_tracknum
    Ui.insert_track dest_block dest_tracknum track

-- * events

events :: Ui.M m => TrackId -> m [Event.Event]
events = fmap Events.ascending . Ui.get_events

selected :: Cmd.M m => m [Event.Event]
selected = do
    (_, _, track_ids, range) <- Selection.tracks
    track_id <- Cmd.require "selected track" (Seq.head track_ids)
    Events.ascending . Events.in_range range <$> Ui.get_events track_id

events_range :: TrackId -> ScoreTime -> ScoreTime -> Cmd.CmdL [Event.Event]
events_range track_id start end =
    Events.ascending . Events.in_range (Events.Range start end) <$>
        Ui.get_events track_id

selected_notation :: Cmd.M m => TrackTime -> m Text
selected_notation step = do
    events <- selected
    (start, end) <- Events.range_times <$> Selection.range
    return $ to_notation start step end events

-- | Reduce event text to notation at a fixed time increment.  It only works
-- out if each event only has a single letter.
to_notation :: TrackTime -> TrackTime -> TrackTime -> [Event.Event] -> Text
to_notation start step end = mconcat . go (Seq.range' start end step)
    where
    go _ [] = []
    go [] ts = replicate (length ts) " "
    go (t:ts) (e:es)
        | Event.start e > t = " " : go ts (e:es)
        | Event.start e == t = Event.text e : go ts es
        | otherwise = " " : go (t:ts) es

-- | 4 measures per line, 16 time steps per measure.
format_measures :: String -> [String]
format_measures = map (List.intercalate "|") . Seq.chunked 4 . Seq.chunked 16


-- * strip controls

drop_dups :: Cmd.CmdL ()
drop_dups = ModifyEvents.selection $ ModifyEvents.events $
    return . Seq.drop_dups Event.text

-- * waveform

waveform :: Cmd.CmdT IO ()
waveform = do
    mapM_ toggle_waveform =<< Selection.track_ids
    -- Invalidate to force it to add or remove waveforms.  It's a big hammer
    -- but it works and it should be uncommon to toggle this.
    Cmd.invalidate_performances

toggle_waveform :: TrackId -> Cmd.CmdT IO ()
toggle_waveform track_id = Ui.modify_waveform track_id not


-- * signal render

filled :: Cmd.CmdL ()
filled = mapM_ (Ui.set_render_style (Track.Filled Nothing))
    =<< Selection.track_ids

line :: Cmd.CmdL ()
line = mapM_ (Ui.set_render_style (Track.Line Nothing))
    =<< Selection.track_ids

note_pitch :: Cmd.CmdL ()
note_pitch = nline "#"

-- | Pass \"#\" for the pitch track.
nline :: Text -> Cmd.CmdL ()
nline = note_render Track.Line

nfilled :: Text -> Cmd.CmdL ()
nfilled = note_render Track.Filled

note_render :: Cmd.M m => (Maybe Track.RenderSource -> Track.RenderStyle)
    -> Text -- ^ Either a control name, or a #-prefixed pitch name.
    -> m ()
note_render mode control_name = do
    control <- Cmd.require_right id $ ScoreT.parse_generic_control control_name
    track_ids <- Selection.track_ids
    track_ids <- filterM is_note track_ids
    mapM_ (Ui.set_render_style
        (mode (Just (either Track.Control Track.Pitch control)))) track_ids
    where
    is_note = fmap ParseTitle.is_note_track . Ui.get_track_title

no_render :: Cmd.CmdL ()
no_render = mapM_ (Ui.set_render_style Track.NoRender) =<< Selection.track_ids
