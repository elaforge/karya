-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds that affect global block config but don't fit into any of the
-- more specefic modules.
module Cmd.BlockConfig where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Info as Info
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Selection as Selection

import qualified Derive.ParseTitle as ParseTitle
import Global
import Types


-- * block

-- | Toggle an edge from the selected parent to the clicked child.
cmd_toggle_edge :: Cmd.M m => Msg.Msg -> m ()
cmd_toggle_edge msg = do
    (block_id, sel_tracknum, _, _) <- Selection.get_insert
    clicked_tracknum <- Cmd.abort_unless $ clicked_track msg
    -- The click order goes in the arrow direction, parent to child.
    let edge = (sel_tracknum, clicked_tracknum)
    success <- Ui.toggle_skeleton_edge False block_id edge
    unless success $
        Log.warn $ "refused to add cycle-creating edge: " <> showt edge
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
toggle_merge_all :: Ui.M m => BlockId -> m ()
toggle_merge_all block_id = toggle_merge block_id =<< Info.block_tracks block_id

toggle_merge_selected :: Cmd.M m => m ()
toggle_merge_selected = do
    (block_id, tracknums) <- Selection.tracknums
    tracks <- Info.block_tracks block_id
    toggle_merge block_id $ filter
        ((`elem` tracknums) . Ui.track_tracknum . Info.track_info) tracks

toggle_merge :: Ui.M m => BlockId -> [Info.Track] -> m ()
toggle_merge block_id tracks =
    ifM (andM (map (track_merged block_id) tracknums))
        (mapM_ (Ui.unmerge_track block_id) tracknums)
        (mapM_ (merge_track block_id) tracknums)
    where
    tracknums = no_parents
        [ Ui.track_tracknum note
        | Info.Track note (Info.Note {}) <- tracks
        ]
    -- A note track parent can't merge, so don't count it.
    no_parents (t1:t2:ts) | t1 + 1 == t2 = no_parents (t2:ts)
    no_parents (t:ts) = t : no_parents ts
    no_parents [] = []

merge_track :: Ui.M m => BlockId -> TrackNum -> m ()
merge_track block_id tracknum =
    whenM (is_control_or_pitch block_id (tracknum + 1)) $
        Ui.merge_track block_id tracknum (tracknum + 1)

track_merged :: Ui.M m => BlockId -> TrackNum -> m Bool
track_merged block_id tracknum = not . Set.null . Block.track_merged <$>
    Ui.get_block_track_at block_id tracknum

is_control_or_pitch :: Ui.M m => BlockId -> TrackNum -> m Bool
is_control_or_pitch block_id tracknum =
    Ui.event_track_at block_id tracknum >>= \x -> case x of
        Nothing -> return False
        Just track_id -> do
            ttype <- ParseTitle.track_type <$> Ui.get_track_title track_id
            return $ ttype
                `elem` [ParseTitle.ControlTrack, ParseTitle.PitchTrack]

cmd_open_block :: Cmd.M m => m ()
cmd_open_block = do
    sel <- Selection.events
    block_id <- Cmd.get_focused_block
    let block_calls = NoteTrack.block_calls (Just block_id) . Event.text
    forM_ sel $ \(_, events) -> forM_ events $ \event ->
        mapM_ Create.view_or_focus =<< block_calls event

cmd_add_block_title :: Cmd.M m => Msg.Msg -> m ()
cmd_add_block_title _ = do
    view_id <- Cmd.get_focused_view
    block_id <- Block.view_block <$> Ui.get_view view_id
    title <- Ui.get_block_title block_id
    when (Text.null title) $
        Ui.set_block_title block_id " "
    Ui.update $ Update.CmdTitleFocus view_id Nothing

-- * collapse / expand tracks

-- | Collapse all the children of this track.
collapse_children :: Ui.M m => BlockId -> TrackId -> m ()
collapse_children block_id track_id = do
    children <- Ui.require ("no children: " <> showt track_id)
        =<< TrackTree.children_of block_id track_id
    forM_ children $ \track -> Ui.add_track_flag
        block_id (Ui.track_tracknum track) Block.Collapse

-- | Expand all collapsed children of this track.  Tracks that were merged
-- when they were collapsed will be left merged.
expand_children :: Ui.M m => BlockId -> TrackId -> m ()
expand_children block_id track_id = do
    children <- Ui.require ("no children: " <> showt track_id)
        =<< TrackTree.children_of block_id track_id
    merged <- mconcatMap Block.track_merged . Block.block_tracks
        <$> Ui.get_block block_id
    forM_ children $ \track ->
        when (Set.member (Ui.track_id track) merged) $
            Ui.remove_track_flag
                block_id (Ui.track_tracknum track) Block.Collapse

-- * merge blocks

append :: Ui.M m => BlockId -> BlockId -> m ()
append dest source = do
    -- By convention the first track is just a ruler.
    tracks <- drop 1 . Block.block_tracks <$> Ui.get_block source
    tracknum <- Ui.track_count dest
    tracknum <- if tracknum <= 1 then return tracknum else do
        Ui.insert_track dest tracknum Block.divider
        return (tracknum + 1)
    forM_ (zip [tracknum..] tracks) $ \(i, track) ->
        Ui.insert_track dest i track
    skel <- Ui.get_skeleton dest
    edges <- Skeleton.flatten <$> Ui.get_skeleton source
    let offset = tracknum - 1 -- -1 because I dropped the first track.
    skel <- Ui.require "couldn't add edges to skel" $
        Skeleton.add_edges [(s+offset, e+offset) | (s, e) <- edges] skel
    Ui.set_skeleton dest skel

-- * track

-- | If the flag is set on any of the selected tracks, unset it.  Otherwise,
-- set it.  This is a bit more complicated than a simple toggle because if
-- you have a collapsed track where one is soloed and one isn't, a simple
-- toggle would just move the solo flag from one track to the other, leaving
-- the track as a whole soloed.
cmd_toggle_flag :: Cmd.M m => Block.TrackFlag -> m ()
cmd_toggle_flag flag = do
    (block_id, tracknums, _, _, _) <- Selection.tracks
    flags <- mapM (Ui.track_flags block_id) tracknums
    let set = any (Set.member flag) flags
    forM_ tracknums $ \tracknum -> if set
        then Ui.remove_track_flag block_id tracknum flag
        else Ui.add_track_flag block_id tracknum flag

cmd_toggle_flag_clicked :: Cmd.M m => Block.TrackFlag -> Msg.Msg -> m ()
cmd_toggle_flag_clicked flag msg = do
    tracknum <- Cmd.abort_unless $ clicked_track msg
    block_id <- Cmd.get_focused_block
    Ui.toggle_track_flag block_id tracknum flag

-- | Enable Solo on the track and disable Mute.  It's bound to a double click
-- so when this cmd fires I have to do undo the results of the single click.
-- Perhaps mute and solo should be exclusive in general.
cmd_set_solo :: Cmd.M m => Msg.Msg -> m ()
cmd_set_solo msg = do
    tracknum <- Cmd.abort_unless $ clicked_track msg
    block_id <- Cmd.get_focused_block
    Ui.remove_track_flag block_id tracknum Block.Mute
    Ui.toggle_track_flag block_id tracknum Block.Solo

-- | Unset solo if it's set, otherwise toggle the mute flag.
cmd_mute_or_unsolo :: Cmd.M m => Msg.Msg -> m ()
cmd_mute_or_unsolo msg = do
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.abort_unless $ clicked_track msg
    flags <- Ui.track_flags block_id tracknum
    if Block.Solo `Set.member` flags
        then Ui.remove_track_flag block_id tracknum Block.Solo
        else Ui.toggle_track_flag block_id tracknum Block.Mute

cmd_expand_track :: Cmd.M m => Msg.Msg -> m ()
cmd_expand_track msg = do
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.abort_unless $ clicked_track msg
    expand_or_unmerge block_id tracknum

expand_or_unmerge :: Ui.M m => BlockId -> TrackNum -> m ()
expand_or_unmerge block_id tracknum = do
    track_id <- Ui.get_event_track_at block_id tracknum
    btracks <- zip [0..] . Block.block_tracks <$> Ui.get_block block_id
    case List.find (Set.member track_id . Block.track_merged . snd) btracks of
        Just (merged_tracknum, _) ->
            Ui.unmerge_track block_id merged_tracknum
        Nothing -> Ui.remove_track_flag block_id tracknum Block.Collapse

-- | Move selected tracks to the left of the clicked track.
cmd_move_tracks :: Cmd.M m => Msg.Msg -> m ()
cmd_move_tracks msg = do
    (block_id, tracknums, _, _, _) <- Selection.tracks
    clicked <- Cmd.abort_unless $ clicked_track msg
    move_tracks block_id tracknums clicked
    -- Shift from the max tracknum or the minimum tracknum, depending on
    -- the move direction.
    whenJust (Seq.minimum_on abs $ map (clicked-) tracknums) $
        Selection.shift False Selection.Move

move_tracks :: Ui.M m => BlockId -> [TrackNum] -> TrackNum -> m ()
move_tracks block_id sources dest =
    mapM_ (uncurry (Ui.move_track block_id)) moves
    where
    moves -- Start at the last source, then insert at the dest counting down.
        | any (<dest) sources =
            zip (List.sortBy (flip compare) sources) [dest, dest-1 ..]
        -- Start at the first source, then insert at the dest counting up.
        | otherwise = zip (List.sort sources) [dest ..]
