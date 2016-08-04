-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Cmds for track level operations.
module Cmd.Repl.LTrack where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection

import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import Global
import Types


-- | List all tracks, along with the number of blocks each one appears in.
list :: Cmd.CmdL [(TrackId, Int)]
list = do
    track_ids <- State.all_track_ids
    counts <- map length <$> mapM State.blocks_with_track_id track_ids
    return $ zip track_ids counts

gc :: Cmd.CmdL [TrackId]
gc = do
    tids <- orphans
    mapM_ State.destroy_track tids
    return tids

-- | Tracks that don't appear in any block.
orphans :: Cmd.CmdL [TrackId]
orphans = Set.elems <$> Create.orphan_tracks

-- | Remove tracks with no events from the given block.
remove_empty :: BlockId -> Cmd.CmdL ()
remove_empty block_id = do
    track_ids <- Block.block_track_ids <$> State.get_block block_id
    mapM_ State.destroy_track =<< filterM is_empty track_ids
    where
    is_empty = fmap ((==Events.empty) . Track.track_events)
        . State.get_track

remove_all_empty :: Cmd.CmdL ()
remove_all_empty = mapM_ remove_empty =<< State.all_block_ids

map_widths :: (Text -> Bool) -> (Types.Width -> Types.Width) -> Cmd.CmdL ()
map_widths wanted f = do
    block_ids <- State.all_block_ids
    forM_ block_ids $ \block_id -> do
        tracknums <- map State.track_tracknum
            . filter (wanted . State.track_title) <$>
                TrackTree.tracks_of block_id
        widths <- map Block.track_width <$>
            mapM (State.get_block_track_at block_id) tracknums
        zipWithM_ (State.set_track_width block_id)
            tracknums (map f widths)

-- | Transform all track titles.
map_titles :: (Text -> Text) -> Cmd.CmdL ()
map_titles f = do
    tids <- State.all_track_ids
    mapM_ (flip State.modify_track_title f) tids

replace :: Text -> Text -> Cmd.CmdL ()
replace from to = map_titles $ Text.replace from to

-- | Find all tracks with the given string in their title.  You can use
-- 'State.blocks_with_track_id' to find the blocks with the tracks, and
-- 'map_titles' or 'replace' to change the titles.
find :: Text -> Cmd.CmdL [(TrackId, Text)]
find search = do
    tids <- State.all_track_ids
    titles <- mapM State.get_track_title tids
    return [(tid, title) | (tid, title) <- zip tids titles,
        search `Text.isInfixOf` title]

-- * manipulation

-- | Duplicate a track from one block to another.  The underlying track is
-- the same, so edits in one of its occurrances will be reflected in all of its
-- blocks.
duplicate :: State.M m => BlockId -> TrackNum -> BlockId -> TrackNum -> m ()
duplicate source_block source_tracknum dest_block dest_tracknum = do
    track <- State.get_block_track_at source_block source_tracknum
    State.insert_track dest_block dest_tracknum track

-- * events

events :: State.M m => TrackId -> m [Event.Event]
events track_id = do
    track <- State.get_track track_id
    return $ Events.ascending $ Track.track_events track

selected :: Cmd.M m => m [Event.Event]
selected = do
    (_, _, track_ids, start, end) <- Selection.tracks
    track_id <- Cmd.require "selected track" (Seq.head track_ids)
    Events.ascending . Events.in_range (Events.Positive start end)
        . Track.track_events <$> State.get_track track_id

events_range :: TrackId -> ScoreTime -> ScoreTime -> Cmd.CmdL [Event.Event]
events_range track_id start end = do
    track <- State.get_track track_id
    return $ Events.ascending $ Events.in_range (Events.Positive start end) $
        Track.track_events track

selected_notation :: Cmd.M m => TrackTime -> m Text
selected_notation step = do
    events <- selected
    (_, _, _, start, end) <- Selection.tracks
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


-- * signal render

filled :: Cmd.CmdL ()
filled = mapM_ (State.set_render_style (Track.Filled Nothing))
    =<< Selection.track_ids

line :: Cmd.CmdL ()
line = mapM_ (State.set_render_style (Track.Line Nothing))
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
    control <- Cmd.require_right id $ Score.parse_generic_control control_name
    track_ids <- Selection.track_ids
    track_ids <- filterM is_note track_ids
    mapM_ (State.set_render_style
        (mode (Just (either Track.Control Track.Pitch control)))) track_ids
    where
    is_note = fmap ParseTitle.is_note_track . State.get_track_title

no_render :: Cmd.CmdL ()
no_render =
    mapM_ (State.set_render_style Track.NoRender) =<< Selection.track_ids
