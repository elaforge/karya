{- | Implement a clipboard, and copy and paste from a selection.

Who knew copy and paste was so complicated?  Copying is complicated because the
structure isn't flat, i.e. a block has references to tracks and rulers.
Pasting is complicated because the clipboard events have to be shifted and
clipped according to the destination selection.

Also, the tracks are typed in that it's not appropriate to paste controller
events into a note track.  However, I'm going to ignore that and assume the
user won't paste them if he didn't mean it.

Instead of having a special case clipboard, the clipboard is implemented as
a set of normal blocks and tracks (rulers are not copied), in a clipboard
namespace.  That way, you can have multiple clipboards by copying them to
different clipboard namespaces, edit clipboards in place, and the paste code is
the same as the code that merges another project from disk.

Further ideas:

- use two selections and a "swap" command
- mouse chording for copy paste
- different mouse buttons are hard to do on the mac, so use standard for now
- merge with function... I think I can just do it in LanguageCmds

More complicated pastes should be implemented as derivers, which are more
flexible than editing operations.  However, there could be a "derive in place"
cmd to flatten deriver structure.
-}
module Cmd.Clip where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit

import qualified App.Config as Config


-- * clipboard ops

-- | Replace the clipboard with the given state.
--
-- TODO If there is an open view on a given block, maybe it can be reopened.
-- Or maybe there can be a setting to automatically open a view on a copied
-- block.
state_to_clip :: (Monad m) => State.State -> Cmd.CmdT m ()
state_to_clip state = state_to_namespace state =<< get_clip_namespace

clear_clip :: (Monad m) => Cmd.CmdT m ()
clear_clip = destroy_namespace =<< get_clip_namespace

-- * copy

-- | Copy events under the current selection into the buffer.
cmd_copy_selection :: (Monad m) => Cmd.CmdT m Cmd.Status
cmd_copy_selection = do
    sel <- copy_selection Config.insert_selnum
    state <- selection_sub_state sel
    state_to_clip state
    return Cmd.Done

-- | Like 'cmd_copy_selection', but shift the following events back by the
-- selection duration.
cmd_cut_selection :: (Monad m) => Cmd.CmdT m Cmd.Status
cmd_cut_selection = do
    cmd_copy_selection
    Edit.cmd_remove_selected

-- | During copies, a point selection is a no-op.
copy_selection :: (Monad m) => Block.SelNum -> Cmd.CmdT m Block.Selection
copy_selection selnum = do
    view_id <- Cmd.get_focused_view
    sel <- Cmd.require =<< State.get_selection view_id selnum
    when (Block.sel_is_point sel) Cmd.abort
    return sel

-- * paste

-- $paste Insert events from the clipboard to the selection. If the selection
-- is a point it's the same as if it extended to the end of the block.

-- | The normal variety of paste that replaces the destination data.
cmd_paste_overwrite :: (Monad m) => Cmd.CmdT m Cmd.Status
cmd_paste_overwrite = do
    (start, end, track_ids, clip_events) <- paste_info
    forM_  (zip track_ids clip_events) $ \(track_id, events) -> do
        State.remove_events track_id start end
        State.insert_events track_id events
    return Cmd.Done

cmd_paste_merge :: (Monad m) => Cmd.CmdT m Cmd.Status
cmd_paste_merge = do
    (_start, _end, track_ids, clip_events) <- paste_info
    forM_  (zip track_ids clip_events) $ \(track_id, events) -> do
        State.insert_events track_id events
    return Cmd.Done

-- | Like 'cmd_paste_merge', except don't merge events that overlap with
-- existing ones.
cmd_paste_soft_merge :: (Monad m) => Cmd.CmdT m Cmd.Status
cmd_paste_soft_merge = do
    (_start, _end, track_ids, clip_events) <- paste_info
    forM_  (zip track_ids clip_events) $ \(track_id, events) -> do
        track_events <- fmap Track.track_events (State.get_track track_id)
        State.insert_events track_id $
            filter (not . overlaps track_events) events
    return Cmd.Done
    where
    overlaps events (pos, _) = Maybe.isJust (Track.event_overlapping events pos)

-- | Insert the events after pushing events after the selection down by
-- the inserted length, which is the minimum of the insert selection and the
-- length of the buffer.
cmd_paste_insert :: (Monad m) => Cmd.CmdT m Cmd.Status
cmd_paste_insert = do
    (start, end, track_ids, clip_events) <- paste_info
    -- Only shift the tracks that are in clip_events.
    mapM_ (shift_track start (end-start)) (map fst (zip track_ids clip_events))
    forM_  (zip track_ids clip_events) $ \(track_id, events) -> do
        State.insert_events track_id events
    return Cmd.Done


-- * implementation

get_clip_block_id :: (Monad m) => Cmd.CmdT m Block.BlockId
get_clip_block_id = do
    clip_ns <- get_clip_namespace
    return $ Block.BlockId (Id.id clip_ns Config.clip_block_name)

-- get_clip_block :: (Monad m) => Cmd.CmdT m (Maybe Block.Block)
-- get_clip_block = do
--     block_id <- get_clip_block_id
--     blocks <- fmap State.state_blocks State.get
--     return $ Map.lookup block_id blocks

-- ** copy

-- | Convert the area under the selection into its own State, consisting of
-- no views, one block, and the tracks under the selection containing only the
-- events in the selection.  The tracks are shifted to start at 1 and the
-- events shifted to start at 0.  The rulers are not copied.
selection_sub_state :: (Monad m) => Block.Selection -> Cmd.CmdT m State.State
selection_sub_state sel = do
    block_id <- Cmd.get_focused_block
    block <- State.get_block block_id

    tracklike_id_widths <- fmap Maybe.catMaybes $
        mapM (State.track_at block_id) (sel_tracknums sel)
    let tracklike_ids = map fst tracklike_id_widths
    tracklikes <- mapM State.get_tracklike tracklike_ids

    clip_block_id <- get_clip_block_id
    State.exec_rethrow "build clip state" State.empty $ do
        -- Inherit everything from the copied block, except the tracks.
        b <- State.create_block (Id.unpack_id clip_block_id) $
            block { Block.block_track_widths = [] }

        let ruler_pairs = Seq.unique_with fst $ zip
                (Block.ruler_ids_of tracklike_ids) (Block.rulers_of tracklikes)
        forM_ ruler_pairs $ \(ruler_id, ruler) ->
            unless (ruler_id == State.no_ruler) $ do
                State.create_ruler (Id.unpack_id ruler_id) ruler
                return ()

        let track_pairs = Seq.unique_with fst $ zip
                (Block.track_ids_of tracklike_ids) (Block.tracks_of tracklikes)
        forM_ track_pairs $ \(track_id, track) ->
            State.create_track (Id.unpack_id track_id) (events_in_sel sel track)
        forM_ (zip [1..] tracklike_id_widths) $ \(n, (tracklike_id, width)) ->
            State.insert_track b n tracklike_id width

-- set_ruler_id rid (Block.TId tid _) = Block.TId tid rid
-- set_ruler_id rid (Block.RId _) = Block.RId rid
-- set_ruler_id _ t = t

events_in_sel sel track =
    track { Track.track_events =
        Track.event_map_asc [(pos-start, evt) | (pos, evt) <- events] }
    where
    (start, end) = Block.sel_range sel
    events = Track.events_in_range start end (Track.track_events track)

-- map_tracklike f (Block.TId tid rid) = Block.TId
--     ((Track.TrackId . f . Track.un_track_id) tid)
--     ((Ruler.RulerId . f . Ruler.un_ruler_id) rid)
-- map_tracklike f (Block.RId rid) =
--     Block.RId ((Ruler.RulerId . f . Ruler.un_ruler_id) rid)
-- map_tracklike _ div@(Block.DId _) = div

-- Move to Block?
sel_tracknums sel = [start .. start + Block.sel_tracks sel - 1]
    where start = Block.sel_start_track sel

-- *** namespace

state_to_namespace :: (State.UiStateMonad m) =>
    State.State -> Id.Namespace -> m ()
state_to_namespace state ns = do
    destroy_namespace ns
    state' <- set_namespace ns state
    global_st <- State.get
    merged <- State.throw_either "merge states"
        (State.merge_states global_st state')
    State.put merged

-- | Set all the IDs in the state to be in the given namespace.  Collisions
-- will throw.
--
-- This is smarter than a State.map_state_ids because it doesn't map the global
-- 'State.no_ruler'.
set_namespace :: (State.UiStateMonad m) => Id.Namespace -> State.State
    -> m State.State
set_namespace ns state = do
    let set ident = Id.id ns name
            where (_, name) = Id.un_id ident
        state2 = state { State.state_rulers = Map.empty }
    State.throw_either "set to clip namespace" $ State.exec state2 $ do
        State.map_view_ids set
        State.map_block_ids set
        State.map_track_ids set
        -- (State.merge_states global_st state')
        -- State.map_ruler_ids $ set_rid

get_clip_namespace :: (Monad m) => Cmd.CmdT m Id.Namespace
get_clip_namespace = fmap Cmd.state_clip_namespace Cmd.get_state

-- | Destroy all views, blocks, tracks, and rulers with the given namespace.
-- TODO move this to Ui.State?
destroy_namespace :: (State.UiStateMonad m) => Id.Namespace -> m ()
destroy_namespace ns = do
    let in_ns = ((==ns) . Id.id_namespace)
    block_ids <- fmap (filter (in_ns . Id.unpack_id))
        State.get_all_block_ids
    blocks <- mapM State.get_block block_ids
    let tracks = concatMap Block.block_tracks blocks
        track_ids = Seq.unique (Maybe.catMaybes (map Block.track_id_of tracks))
        ruler_ids = Seq.unique (Maybe.catMaybes (map Block.ruler_id_of tracks))
    -- Will destroy any views too.
    mapM_ State.destroy_block block_ids
    mapM_ State.destroy_track (filter (in_ns . Id.unpack_id) track_ids)
    mapM_ State.destroy_ruler (filter (in_ns . Id.unpack_id) ruler_ids)

-- ** paste

-- | Get the info necessary to paste from the clipboard: start and end pos,
-- the tracks in the destination selection, and the events from the clipboard
-- grouped by track.  The clipboard events are clipped to start--end.
paste_info :: (Monad m) =>
    Cmd.CmdT m (TrackPos, TrackPos, [Track.TrackId], [[Track.PosEvent]])
paste_info = do
    (track_ids, clip_track_ids, sel) <- get_paste_area
    let (start, end) = Block.sel_range sel
    clip_events <- mapM (clip_track_events start end) clip_track_ids
    return (start, end, track_ids, clip_events)

clip_track_events start end track_id = do
    track <- State.get_track track_id
    let events = Track.clip_to_range
            (TrackPos 0) (end-start) (Track.track_events track)
        shifted = map (\(pos, evt) -> (pos+start, evt)) events
    return shifted

-- | Get the destination and clip tracks involved in a paste, along with the
-- paste selection.
--
-- During pastes, a point selection extends to the end of the last pasted
-- event.
get_paste_area :: (Monad m) =>
    Cmd.CmdT m ([Track.TrackId], [Track.TrackId], Block.Selection)
get_paste_area = do
    view_id <- Cmd.get_focused_view
    sel <- Cmd.require =<< State.get_selection view_id Config.insert_selnum
    clip_block_id <- get_clip_block_id
    clip_block <- State.get_block clip_block_id
    -- If the clip block has any rulers or anything, I skip them.
    let clip_track_ids = take (Block.sel_tracks sel)
            (Block.track_ids_of (Block.block_tracks clip_block))
    clip_end <- State.event_end clip_block_id
    sel <- return $ if Block.sel_is_point sel
        then sel { Block.sel_duration = clip_end }
        else sel

    block_id <- Cmd.get_focused_block
    block <- State.get_block block_id
    let track_ids = Block.track_ids_of
            (drop (Block.sel_start_track sel) (Block.block_tracks block))
    return (track_ids, clip_track_ids, sel)

shift_track start shift track_id = do
    track <- State.get_track track_id
    let shifted = shift_from start shift (Track.track_events track)
    State.set_events track_id shifted

-- | All events starting at a point to the end are shifted by the given amount.
shift_from :: TrackPos -> TrackPos -> Track.TrackEvents -> Track.TrackEvents
shift_from start shift events = merged
    where
    end = Track.time_end events
    shifted = map (\(pos, evt) -> (pos+shift, evt)) (Track.forward start events)
    merged = Track.insert_events shifted (Track.remove_events start end events)
