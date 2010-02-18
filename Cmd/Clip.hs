{- | Implement a clipboard, and copy and paste from a selection.

    Who knew copy and paste was so complicated?  Copying is complicated because
    the structure isn't flat, i.e. a block has references to tracks and rulers.
    Pasting is complicated because the clipboard events have to be shifted and
    clipped according to the destination selection.

    Also, the tracks are typed in that it's not appropriate to paste control
    events into a note track.  However, I'm going to ignore that and assume the
    user won't paste them if he didn't mean it.

    Instead of having a special case clipboard, the clipboard is implemented as
    a set of normal blocks and tracks (rulers are not copied), in a clipboard
    namespace.  That way, you can have multiple clipboards by copying them to
    different clipboard namespaces, edit clipboards in place, and the paste
    code is the same as the code that merges another project from disk.

    Further ideas:

    - use two selections and a "swap" command

    - mouse chording for copy paste

    - different mouse buttons are hard to do on the mac, so use standard for now

    - merge with function... I think I can just do it in LanguageCmds

    More complicated pastes should be implemented as derivers, which are more
    flexible than editing operations.  However, there could be a "derive in
    place" cmd to flatten deriver structure.
-}
module Cmd.Clip where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.ModifyEvents as ModifyEvents

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
cmd_copy_selection :: (Monad m) => Cmd.CmdT m ()
cmd_copy_selection = do
    sel <- copy_selection Config.insert_selnum
    state <- selection_sub_state sel
    state_to_clip state

-- | Like 'cmd_copy_selection', but shift the following events back by the
-- selection duration.
cmd_cut_selection :: (Monad m) => Cmd.CmdT m ()
cmd_cut_selection = do
    cmd_copy_selection
    Edit.cmd_clear_selected

-- | During copies, a point selection is a no-op.
copy_selection :: (Monad m) => Types.SelNum -> Cmd.CmdT m Types.Selection
copy_selection selnum = do
    view_id <- Cmd.get_focused_view
    sel <- Cmd.require =<< State.get_selection view_id selnum
    when (Types.sel_is_point sel) Cmd.abort
    return sel

-- * paste

-- $paste Insert events from the clipboard to the selection. If the selection
-- is a point it's the same as if it extended to the end of the block.

-- | The normal variety of paste that replaces the destination data.
cmd_paste_overwrite :: (Monad m) => Cmd.CmdT m ()
cmd_paste_overwrite = do
    (start, end, track_ids, clip_events) <- paste_info
    forM_  (zip track_ids clip_events) $ \(track_id, events) -> do
        State.remove_events track_id start end
        State.insert_events track_id events

cmd_paste_merge :: (Monad m) => Cmd.CmdT m ()
cmd_paste_merge = do
    (_start, _end, track_ids, clip_events) <- paste_info
    forM_  (zip track_ids clip_events) $ \(track_id, events) -> do
        State.insert_events track_id events

-- | Like 'cmd_paste_merge', except don't merge events that overlap with
-- existing ones.
cmd_paste_soft_merge :: (Monad m) => Cmd.CmdT m ()
cmd_paste_soft_merge = do
    (_start, _end, track_ids, clip_events) <- paste_info
    forM_  (zip track_ids clip_events) $ \(track_id, events) -> do
        track_events <- fmap Track.track_events (State.get_track track_id)
        State.insert_events track_id $
            filter (not . overlaps track_events) events
    where
    overlaps events (pos, _) = Maybe.isJust (Track.event_overlapping pos events)

-- | Insert the events after pushing events after the selection down by
-- the inserted length, which is the minimum of the insert selection and the
-- length of the buffer.
cmd_paste_insert :: (Monad m) => Cmd.CmdT m ()
cmd_paste_insert = do
    (start, end, track_ids, clip_events) <- paste_info
    -- Only shift the tracks that are in clip_events.
    mapM_ (ModifyEvents.move_track_events start (end-start))
        (map fst (zip track_ids clip_events))
    forM_  (zip track_ids clip_events) $ \(track_id, events) -> do
        State.insert_events track_id events


-- * implementation

get_clip_block_id :: (Monad m) => Cmd.CmdT m BlockId
get_clip_block_id = do
    clip_ns <- get_clip_namespace
    return $ Types.BlockId (Id.id clip_ns Config.clip_block_name)

-- ** copy

-- | Convert the area under the selection into its own State, consisting of
-- no views, one block, and the tracks under the selection containing only the
-- events in the selection.  The tracks are shifted to start at 1 and the
-- events shifted to start at 0.
--
-- The rulers are not copied because it doesn't really make sense to paste
-- them, and when the events get shifted they probably won't line up anyway.
-- TODO On the other hand, it can be convenient to edit the clipboard in-place,
-- and it's nice to have a ruler for that.  But it's not too hard to just copy
-- a ruler over, so I'll wait and see what experience shows.
--
-- Also strip out the other track attributes, like hidden, muted, etc.
selection_sub_state :: (Monad m) => Types.Selection -> Cmd.CmdT m State.State
selection_sub_state sel = do
    block_id <- Cmd.get_focused_block
    block <- State.get_block block_id

    tracks <- fmap Maybe.catMaybes $
        mapM (State.block_track_at block_id) (Types.sel_tracknums sel)
    let tracklike_ids = map Block.tracklike_id tracks
    tracklikes <- mapM State.get_tracklike tracklike_ids

    clip_block_id <- get_clip_block_id
    State.exec_rethrow "build clip state" State.empty $ do
        -- Inherit everything from the copied block, except the tracks.
        b <- State.create_block (Id.unpack_id clip_block_id) $
            block { Block.block_tracks = [] }

        -- Copy over the tracks, but without their rulers.
        let track_pairs = Seq.unique_on fst $ zip
                (Block.track_ids_of tracklike_ids) (Block.tracks_of tracklikes)
        forM_ track_pairs $ \(track_id, track) ->
            State.create_track (Id.unpack_id track_id) (events_in_sel sel track)
        forM_ (zip [1..] tracks) $ \(n, track) ->
            State.insert_track b n $ Block.block_track
                (Block.set_rid State.no_ruler (Block.tracklike_id track))
                (Block.track_width track)

-- | Make a copy of the track with only the events in the selection shifted
-- by the offset of the selection.
events_in_sel :: Types.Selection -> Track.Track -> Track.Track
events_in_sel sel track =
    track { Track.track_events =
        Track.event_map_asc [(pos-start, evt) | (pos, evt) <- events] }
    where
    (start, end) = Types.sel_range sel
    events = Track.events_in_range start end (Track.track_events track)

-- *** namespace

state_to_namespace :: (State.UiStateMonad m) =>
    State.State -> Id.Namespace -> m ()
state_to_namespace state ns = do
    destroy_namespace ns
    state2 <- set_namespace ns state
    global_st <- State.get
    merged <- State.throw_either "merge states"
        (State.merge_states global_st state2)
    State.put merged

-- | Set all the IDs in the state to be in the given namespace, except rulers.
-- Collisions will throw.  Rulers are omitted because copy and paste doesn't
-- mess with rulers.
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

get_clip_namespace :: (Monad m) => Cmd.CmdT m Id.Namespace
get_clip_namespace = Cmd.gets Cmd.state_clip_namespace

-- | Destroy all views, blocks, tracks, and rulers with the given namespace.
-- TODO move this to Ui.State?
destroy_namespace :: (State.UiStateMonad m) => Id.Namespace -> m ()
destroy_namespace ns = do
    let in_ns = ((==ns) . Id.id_namespace)
    block_ids <- fmap (filter (in_ns . Id.unpack_id))
        State.get_all_block_ids
    blocks <- mapM State.get_block block_ids
    let track_ids = Seq.unique $ concatMap Block.block_track_ids blocks
        ruler_ids = Seq.unique $ concatMap Block.block_ruler_ids blocks
    -- Will destroy any views too.
    mapM_ State.destroy_block block_ids
    mapM_ State.destroy_track (filter (in_ns . Id.unpack_id) track_ids)
    mapM_ State.destroy_ruler (filter (in_ns . Id.unpack_id) ruler_ids)

-- ** paste

-- | Get the info necessary to paste from the clipboard: start and end pos,
-- the tracks in the destination selection, and the events from the clipboard
-- grouped by track.  The clipboard events are clipped to start--end and
-- shifted into the paste range.
paste_info :: (Monad m) =>
    Cmd.CmdT m (ScoreTime, ScoreTime, [TrackId], [[Track.PosEvent]])
paste_info = do
    (track_ids, clip_track_ids, sel) <- get_paste_area
    let (start, end) = Types.sel_range sel
    clip_events <- mapM (clip_track_events start end) clip_track_ids
    return (start, end, track_ids, clip_events)

clip_track_events :: (State.UiStateMonad m) =>
    ScoreTime -> ScoreTime -> TrackId -> m [Track.PosEvent]
clip_track_events start end track_id = do
    track <- State.get_track track_id
    let events = clip_events (end-start)
            (Track.event_list (Track.track_events track))
        shifted = map (\(pos, evt) -> (pos+start, evt)) events
    return shifted

clip_events :: ScoreTime -> [Track.PosEvent] -> [Track.PosEvent]
clip_events _ [] = []
clip_events point (event@(pos, evt):events)
    | pos >= point = []
    | Track.event_end event > point =
        [(pos, Event.modify_duration (\d -> min d (point - pos)) evt)]
    | otherwise = event : clip_events point events

-- | Get the destination and clip tracks involved in a paste, along with the
-- paste selection.
--
-- During pastes, a point selection extends to the end of the last pasted
-- event.
get_paste_area :: (Monad m) =>
    Cmd.CmdT m ([TrackId], [TrackId], Types.Selection)
get_paste_area = do
    view_id <- Cmd.get_focused_view
    sel <- Cmd.require =<< State.get_selection view_id Config.insert_selnum
    clip_block_id <- get_clip_block_id
    clip_block <- State.get_block clip_block_id

    -- If the clip block has any rulers or anything, I skip them.
    let clip_track_ids = take (length (Types.sel_tracknums sel))
            (Block.block_track_ids clip_block)
    clip_end <- State.event_end clip_block_id
    sel <- return $ if Types.sel_is_point sel
        then Types.sel_set_duration clip_end sel
        else sel

    block_id <- Cmd.get_focused_block
    block <- State.get_block block_id
    let track_ids = Block.track_ids_of $ map Block.tracklike_id $
            drop (Types.sel_start_track sel) (Block.block_tracks block)
    return (track_ids, clip_track_ids, sel)
