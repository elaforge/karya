-- | Cmds that affect global block config but don't fit into any of the
-- more specefic modules.
module Cmd.BlockConfig where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Info as Info
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Selection as Selection
import qualified Cmd.ViewConfig as ViewConfig

import qualified Derive.TrackInfo as TrackInfo
import qualified App.Config as Config
import Types


-- * block

cmd_toggle_edge :: (Cmd.M m) => Msg.Msg -> m ()
cmd_toggle_edge msg = do
    (block_id, sel_tracknum, _, _) <- Selection.get_insert
    clicked_tracknum <- Cmd.require $ clicked_track msg
    -- The click order goes in the arrow direction, caller-to-callee.
    let edge = (sel_tracknum, clicked_tracknum)
    success <- State.toggle_skeleton_edge block_id edge
    unless success $
        Log.warn $ "refused to add cycle-creating edge: " ++ show edge
    -- The shift below is incorrect.  Anyway, a common case is to splice
    -- a track above and then delete the unwanted edges, and moving the
    -- selection makes that inconvenient.
    -- let shift = clicked_tracknum - sel_tracknum
    -- if success
    --     then Selection.cmd_shift_selection Config.insert_selnum shift False
    --     else Log.warn $ "refused to add cycle-creating edge: " ++ show edge

clicked_track :: Msg.Msg -> Maybe TrackNum
clicked_track msg = case (Msg.mouse_down msg, Msg.context_track msg) of
    (True, Just (tracknum, _)) -> Just tracknum
    _ -> Nothing

-- | Merge all adjacent note/pitch pairs.  If they're already all merged,
-- unmerge them all.
toggle_merge_all :: (State.M m) => BlockId -> m ()
toggle_merge_all block_id = do
    tracks <- Info.block_tracks block_id
    let note_pitches = do
            Info.Track note (Info.Note controls) <- tracks
            pitch <- maybe [] (:[]) $ List.find
                (TrackInfo.is_pitch_track . State.track_title) controls
            return (State.track_tracknum note, State.track_tracknum pitch)
    ifM (andM [track_merged block_id tracknum | (tracknum, _) <- note_pitches])
        (mapM_ (State.unmerge_track block_id) (map fst note_pitches))
        (mapM_ (uncurry (State.merge_track block_id)) note_pitches)

track_merged :: (State.M m) => BlockId -> TrackNum -> m Bool
track_merged block_id tracknum =
    not . null . Block.track_merged <$> State.get_block_track block_id tracknum

cmd_open_block :: (Cmd.M m) => m ()
cmd_open_block = do
    ns <- State.get_namespace
    let call_of = NoteTrack.block_call ns
    sel <- Selection.events
    forM_ sel $ \(_, _, events) -> forM_ events $ \(_, event) ->
        when_just (call_of (Event.event_string event)) $ \block_id ->
            whenM (Maybe.isJust <$> State.lookup_block block_id) $ do
                views <- State.get_views_of block_id
                maybe (Create.fitted_view block_id >> return ())
                    ViewConfig.bring_to_front (Seq.head (Map.keys views))

-- * track

cmd_toggle_flag :: (Cmd.M m) => Block.TrackFlag -> m ()
cmd_toggle_flag flag = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    State.toggle_track_flag block_id tracknum flag

cmd_expand_track :: (Cmd.M m) => Msg.Msg -> m ()
cmd_expand_track msg = do
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.require (clicked_track msg)
    State.remove_track_flag block_id tracknum Block.Collapse

-- | Move selected tracks to the left of the clicked track.
cmd_move_tracks :: (Cmd.M m) => Msg.Msg -> m ()
cmd_move_tracks msg = do
    (tracknums, _, _, _) <- Selection.tracks
    block_id <- Cmd.get_focused_block
    clicked_tracknum <- Cmd.require $ clicked_track msg
    tracks <- State.tracks block_id
    let to = min clicked_tracknum (tracks - length tracknums)
    from <- Cmd.require (Seq.head tracknums)
    let shift = to - from
        moves = (if shift > 0 then reverse else id) (zip tracknums [to..])
    mapM_ (uncurry (move_track block_id)) moves
    Selection.cmd_shift_selection Config.insert_selnum shift False

move_track :: (State.M m) => BlockId -> TrackNum -> TrackNum -> m ()
move_track block_id from to = do
    block <- State.get_block block_id
    let msg = "move_track: from index " ++ show from ++ " out of range"
    State.modify_block block_id . const =<< State.require msg
        (move_block_track from to block)

move_block_track :: TrackNum -> TrackNum -> Block.Block -> Maybe Block.Block
move_block_track from to block = do
    tracks <- Seq.move from to (Block.block_tracks block)
    skel <- Skeleton.move from to (Block.block_skeleton block)
    return $ block
        { Block.block_tracks = tracks, Block.block_skeleton = skel }
