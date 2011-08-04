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

    - merge with function... I think I can just do it at the REPL

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
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Transform as Transform
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection

import qualified App.Config as Config


-- * clipboard ops

-- | Replace the clipboard with the given state.
--
-- TODO If there is an open view on a given block, maybe it can be reopened.
-- Or maybe there can be a setting to automatically open a view on a copied
-- block.
state_to_clip :: (Cmd.M m) => State.State -> m ()
state_to_clip state = state_to_namespace state =<< get_clip_namespace

clear_clip :: (Cmd.M m) => m ()
clear_clip = destroy_namespace =<< get_clip_namespace

-- * copy

-- | Copy events under the current selection into the buffer.
cmd_copy_selection :: (Cmd.M m) => m ()
cmd_copy_selection = do
    sel <- copy_selection Config.insert_selnum
    state <- selection_sub_state sel
    state_to_clip state

-- | Like 'cmd_copy_selection', but shift the following events back by the
-- selection duration.
cmd_cut_selection :: (Cmd.M m) => m ()
cmd_cut_selection = do
    cmd_copy_selection
    Edit.cmd_clear_selected

copy_selection :: (Cmd.M m) => Types.SelNum -> m Selection.SelectedTracks
copy_selection selnum = do
    sel@(_, _, start, end) <- Selection.tracks_selnum selnum
    when (start == end) Cmd.abort
    return sel

-- * paste

-- $paste Insert events from the clipboard to the selection. If the selection
-- is a point it's the same as if it extended to the end of the block.

-- | The normal variety of paste that replaces the destination data.
cmd_paste_overwrite :: (Cmd.M m) => m ()
cmd_paste_overwrite = do
    (start, end, track_ids, clip_events) <- paste_info
    forM_  (zip track_ids clip_events) $ \(track_id, events) -> do
        State.remove_events track_id start end
        State.insert_events track_id events

cmd_paste_merge :: (Cmd.M m) => m ()
cmd_paste_merge = do
    (_start, _end, track_ids, clip_events) <- paste_info
    forM_  (zip track_ids clip_events) $ \(track_id, events) ->
        State.insert_events track_id events

-- | Like 'cmd_paste_merge', except don't merge events that overlap with
-- existing ones.
cmd_paste_soft_merge :: (Cmd.M m) => m ()
cmd_paste_soft_merge = do
    (_start, _end, track_ids, clip_events) <- paste_info
    forM_  (zip track_ids clip_events) $ \(track_id, events) -> do
        track_events <- fmap Track.track_events (State.get_track track_id)
        State.insert_events track_id $
            filter (not . overlaps track_events) events
    where
    overlaps events (pos, _) = Maybe.isJust (Events.overlapping pos events)

-- | Insert the events after pushing events after the selection down by
-- the inserted length, which is the minimum of the insert selection and the
-- length of the buffer.
cmd_paste_insert :: (Cmd.M m) => m ()
cmd_paste_insert = do
    (start, end, track_ids, clip_events) <- paste_info
    -- Only shift the tracks that are in clip_events.
    mapM_ (ModifyEvents.move_track_events start (end-start))
        (map fst (zip track_ids clip_events))
    forM_  (zip track_ids clip_events) $ \(track_id, events) ->
        State.insert_events track_id events


-- * implementation

get_clip_block_id :: (Cmd.M m) => m BlockId
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
-- Also strip out the skeleton and other track attributes, like hidden, muted,
-- etc.
selection_sub_state :: (Cmd.M m) => Selection.SelectedTracks -> m State.State
selection_sub_state (tracknums, _, start, end) = do
    block_id <- Cmd.get_focused_block
    block <- State.get_block block_id

    tracks <- fmap Maybe.catMaybes $
        mapM (State.block_track_at block_id) tracknums
    let tracklike_ids = map Block.tracklike_id tracks
    tracklikes <- mapM State.get_tracklike tracklike_ids

    clip_block_id <- get_clip_block_id
    State.exec_rethrow "build clip state" State.empty $ do
        -- Inherit everything from the copied block, except the tracks and the
        -- skeleton.
        b <- State.create_block (Id.unpack_id clip_block_id) $ block
            { Block.block_tracks = []
            , Block.block_skeleton = Skeleton.empty
            }

        -- Copy over the tracks, but without their rulers.
        let track_pairs = Seq.unique_on fst $ zip
                (Block.track_ids_of tracklike_ids) (Block.tracks_of tracklikes)
        forM_ track_pairs $ \(track_id, track) ->
            State.create_track (Id.unpack_id track_id)
                (events_in_range start end track)
        forM_ (zip [1..] tracks) $ \(n, track) ->
            State.insert_track b n $ Block.track
                (Block.set_rid State.no_ruler (Block.tracklike_id track))
                (Block.track_width track)

-- | Make a copy of the track with only the events in the selection shifted
-- by the offset of the selection.
events_in_range :: ScoreTime -> ScoreTime -> Track.Track -> Track.Track
events_in_range start end track =
    track { Track.track_events = shift (Track.track_events track) }
    where
    shift = Events.map_sorted (\(pos, evt) -> (pos-start, evt))
        . Events.in_range start end

-- *** namespace

-- | Rename the blocks and tracks in the given state into the given namespace
-- and replace the IDs already in that namespace with it.  Rulers are ignored.
--
-- This means that if the given state has IDs in more than one namespace, they
-- will be flattened into one.  Any collisions will throw an exception.
state_to_namespace :: (State.M m) => State.State -> Id.Namespace -> m ()
state_to_namespace state ns = do
    destroy_namespace ns
    state2 <- set_namespace ns state
    global_st <- State.get
    merged <- State.throw_either "merge states"
        (Transform.merge_states global_st state2)
    State.put merged

-- | Set all the IDs in the state to be in the given namespace, except rulers.
-- Collisions will throw.  Rulers are omitted because copy and paste doesn't
-- mess with rulers.
set_namespace :: (State.M m) => Id.Namespace -> State.State -> m State.State
set_namespace ns state = do
    let set ident = Id.id ns name
            where (_, name) = Id.un_id ident
        state2 = state { State.state_rulers = Map.empty }
    State.throw_either "set to clip namespace" $ State.exec state2 $ do
        Transform.map_view_ids set
        Transform.map_block_ids set
        Transform.map_track_ids set

get_clip_namespace :: (Cmd.M m) => m Id.Namespace
get_clip_namespace = Cmd.gets Cmd.state_clip_namespace

-- | Destroy all views, blocks, tracks, and rulers with the given namespace.
-- TODO move this to Ui.State?
destroy_namespace :: (State.M m) => Id.Namespace -> m ()
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
paste_info :: (Cmd.M m) =>
    m (ScoreTime, ScoreTime, [TrackId], [[Events.PosEvent]])
paste_info = do
    (track_ids, clip_track_ids, start, end) <- get_paste_area
    clip_events <- mapM (clip_track_events start end) clip_track_ids
    return (start, end, track_ids, clip_events)

clip_track_events :: (State.M m) =>
    ScoreTime -> ScoreTime -> TrackId -> m [Events.PosEvent]
clip_track_events start end track_id = do
    track <- State.get_track track_id
    let events = clip_events (end-start)
            (Events.ascending (Track.track_events track))
        shifted = map (\(pos, evt) -> (pos+start, evt)) events
    return shifted

clip_events :: ScoreTime -> [Events.PosEvent] -> [Events.PosEvent]
clip_events _ [] = []
clip_events point (event@(pos, evt):events)
    | pos >= point = []
    | Events.end event > point =
        [(pos, Event.modify_duration (\d -> min d (point - pos)) evt)]
    | otherwise = event : clip_events point events

-- | Get the destination and clip tracks involved in a paste, along with the
-- paste selection.
--
-- During pastes, a point selection extends to the end of the last pasted
-- event.
get_paste_area :: (Cmd.M m) => m ([TrackId], [TrackId], ScoreTime, ScoreTime)
get_paste_area = do
    (tracknums, track_ids, start, end) <- Selection.tracks
    clip_block_id <- get_clip_block_id
    clip_block <- State.get_block clip_block_id
    -- If the clip block has any rulers or anything, I skip them.
    let clip_track_ids =
            take (length tracknums) (Block.block_track_ids clip_block)
    clip_end <- State.block_event_end clip_block_id
    return (track_ids, clip_track_ids, start,
        if start == end then start + clip_end else end)
