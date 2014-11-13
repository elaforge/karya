-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Cmds for track level operations.
module Cmd.Repl.LTrack where
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
import qualified Cmd.PlayUtil as PlayUtil
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

events :: TrackId -> ScoreTime -> ScoreTime -> Cmd.CmdL [Event.Event]
events track_id start end = do
    track <- State.get_track track_id
    return $ (Events.ascending
        . Events.in_range start end . Track.track_events) track

-- * strip controls

drop_dups :: Cmd.CmdL ()
drop_dups = ModifyEvents.selection $ ModifyEvents.events $
    return . Seq.drop_dups Event.event_text


-- * signal render

filled :: Cmd.CmdL ()
filled = do
    (block_id, _, track_ids, _, _) <- Selection.tracks
    PlayUtil.clear_cache block_id
    mapM_ (State.set_render_style (Track.Filled Nothing)) track_ids

line :: Cmd.CmdL ()
line = do
    (block_id, _, track_ids, _, _) <- Selection.tracks
    PlayUtil.clear_cache block_id
    mapM_ (State.set_render_style (Track.Line Nothing)) track_ids

-- | Just # for the pitch track.
nline :: Text -> Cmd.CmdL ()
nline = note_render Track.Line

nfilled :: Text -> Cmd.CmdL ()
nfilled = note_render Track.Filled

note_render :: Cmd.M m => (Maybe Track.RenderSource -> Track.RenderStyle)
    -> Text -- ^ Either a control name, or a #-prefixed pitch name.
    -> m ()
note_render mode control_name = do
    (block_id, _, track_ids, _, _) <- Selection.tracks
    PlayUtil.clear_cache block_id
    track_ids <- filterM is_note track_ids
    mapM_ (State.set_render_style (mode (Just control))) track_ids
    where
    control = case Text.stripPrefix "#" control_name of
        Nothing -> Track.Control $ Score.control control_name
        Just s
            | Text.null s -> Track.Pitch Nothing
            | otherwise -> Track.Pitch $ Just (Score.control s)
    is_note = fmap ParseTitle.is_note_track . State.get_track_title

no_render :: Cmd.CmdL ()
no_render = do
    (block_id, _, track_ids, _, _) <- Selection.tracks
    PlayUtil.clear_cache block_id
    mapM_ (State.set_render_style Track.NoRender) track_ids
