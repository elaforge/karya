-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds that affect global block config but don't fit into any of the
-- more specefic modules.
module Cmd.BlockConfig (
    cmd_toggle_edge
    , toggle_merge_all
    , toggle_merge_selected
    , cmd_open_block
    , cmd_add_block_title
    , clip
    , collapse_children
    , expand_children
    , append
    , cmd_toggle_flag
    , toggle_collapse_empty
    , cmd_set_solo
    , cmd_mute_or_unsolo
    , cmd_expand_track
    , cmd_move_tracks
    , move_tracks
) where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import qualified Util.Log as Log
import qualified Util.Rect as Rect
import qualified Util.Lists as Lists
import qualified Util.Trees as Trees

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Info as Info
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteTrackParse as NoteTrackParse
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Selection as Selection
import qualified Cmd.Views as Views

import qualified Derive.ParseTitle as ParseTitle
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Skeleton as Skeleton
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.Update as Update

import           Global
import           Types


-- * skeleton

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

-- * merge

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
toggle_merge block_id tracks = do
    tracknums <- mergeable block_id $ no_parents
        [ Ui.track_tracknum note
        | Info.Track note (Info.Note {}) <- tracks
        ]
    ifM (allM (Ui.track_merged block_id) tracknums)
        (mapM_ (Ui.unmerge_track block_id) tracknums)
        (mapM_ (\t -> Ui.merge_track block_id t (t+1)) tracknums)
    where
    -- A note track parent can't merge, so don't count it.
    no_parents (t1:t2:ts) | t1 + 1 == t2 = no_parents (t2:ts)
    no_parents (t:ts) = t : no_parents ts
    no_parents [] = []

mergeable :: Ui.M m => BlockId -> [TrackNum] -> m [TrackNum]
mergeable block_id =
    filterM $ \tracknum -> is_control_or_pitch tracknum block_id (tracknum + 1)

-- | True if the track is a control or pitch track, and a child of the given
-- tracknum.
is_control_or_pitch :: Ui.M m => TrackNum -> BlockId -> TrackNum -> m Bool
is_control_or_pitch parent block_id tracknum =
    Ui.event_track_at block_id tracknum >>= \case
        Nothing -> return False
        Just track_id -> andM
            [ (`elem` [ParseTitle.ControlTrack, ParseTitle.PitchTrack]) <$>
                ParseTitle.track_type <$> Ui.get_track_title track_id
            , TrackTree.is_child_of block_id parent tracknum
            ]

-- * misc

cmd_open_block :: Cmd.M m => Bool -> m ()
cmd_open_block align_new_view = do
    sel <- Selection.events
    block_id <- Cmd.get_focused_block
    parent <- Ui.get_view =<< Cmd.get_focused_view
    let block_calls = NoteTrackParse.expr_block_calls True block_id . Event.text
    forM_ sel $ \(_, events) -> forM_ events $ \event ->
        mapM_ (open parent event) =<< block_calls event
    where
    open parent event block_id = do
        new_view <- Create.view_or_focus block_id
        when align_new_view $ whenJust new_view $ \view_id ->
            align_view_to parent (Event.start event) (Event.end event) view_id

-- | Line the ViewId up to be right next to the given parent view and fit into
-- the given time range.
align_view_to :: Cmd.M m => Block.View -> TrackTime -> TrackTime -> ViewId
    -> m ()
align_view_to parent start end view_id = do
    let x = Rect.r $ Block.view_rect parent
        top = Block.screen_pixels parent start
        bottom = Block.screen_pixels parent end
    width <- Rect.w . Block.track_rect <$> Ui.get_view view_id
    Views.set_track_rect view_id $ Rect.xywh x top width (bottom - top)
    Views.zoom_to_ruler view_id

-- | Add an empty title and focus on it.  The text widget hides itself when
-- empty, so the space gets it to appear for focus.
cmd_add_block_title :: Cmd.M m => Msg.Msg -> m ()
cmd_add_block_title _ = do
    view_id <- Cmd.get_focused_view
    block_id <- Block.view_block <$> Ui.get_view view_id
    title <- Ui.get_block_title block_id
    when (Text.null title) $
        Ui.set_block_title block_id " "
    Ui.damage $ mempty { Update._title_focus = Just (view_id, Nothing) }

-- | Clip a block to the selection.
clip :: Cmd.M m => m ()
clip = do
    (block_id, _, _, pos) <- Selection.get_insert
    clip_to block_id pos

clip_to :: Ui.M m => BlockId -> TrackTime -> m ()
clip_to block_id end = do
    RulerUtil.local_meter RulerUtil.Block block_id $
        Meter.modify_sections (Meter.sections_take end)
    track_ids <- Ui.track_ids_of block_id
    forM_ track_ids $ \track_id ->
        Ui.modify_events track_id $ Events.clip True end

-- * collapse / expand tracks

-- | Collapse all the children of this track.
collapse_children :: Ui.M m => BlockId -> TrackId -> m ()
collapse_children block_id track_id = do
    children <- TrackTree.get_children_of block_id track_id
    forM_ children $ \track -> Ui.add_track_flag
        block_id (Ui.track_tracknum track) Block.Collapse

-- | Expand all collapsed children of this track.  Tracks that were merged
-- when they were collapsed will be left merged.
expand_children :: Ui.M m => BlockId -> TrackId -> m ()
expand_children block_id track_id = do
    children <- TrackTree.get_children_of block_id track_id
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
    (block_id, tracknums, _, _) <- Selection.tracks
    flags <- mapM (Ui.track_flags block_id) tracknums
    let set = any (Set.member flag) flags
    forM_ tracknums $ \tracknum -> if set
        then Ui.remove_track_flag block_id tracknum flag
        else Ui.add_track_flag block_id tracknum flag

-- | Collapse all tracks that are empty and have all empty children.  If
-- everything empty is collapsed, expand them all.  Don't consider
-- merge-collapsed.
toggle_collapse_empty :: Ui.M m => BlockId -> m ()
toggle_collapse_empty block_id = do
    tracks <- TrackTree.track_tree_of block_id
    tracks <- traverse (traverse (\t -> (t,) <$> is_empty t)) tracks
    let empty = concatMap empty_tracks tracks
    all_collapsed <- allM is_collapsed empty
    forM_ (map Ui.track_tracknum empty) $ \tracknum -> if all_collapsed
        then Ui.remove_track_flag block_id tracknum Block.Collapse
        else Ui.add_track_flag block_id tracknum Block.Collapse
    where
    is_empty = fmap Events.null . Ui.get_events . Ui.track_id
    is_collapsed = fmap Block.is_collapsed . Ui.track_flags block_id
        . Ui.track_tracknum
    empty_tracks (Tree.Node (track, empty) children)
        | empty && all snd leaves = track : map fst leaves
        | otherwise = concatMap empty_tracks children
        where leaves = concatMap Trees.leaves children

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
    expand_track block_id tracknum

-- | Expand collapsed track.  If it's Merged, try to expand the one to the
-- left.  If a track with a merged is collapsed, this let's me click on either
-- to expand.
expand_track :: Ui.M m => BlockId -> TrackNum -> m ()
expand_track block_id tracknum = do
    flags <- Ui.track_flags block_id tracknum
    Ui.remove_track_flag block_id
        (if Set.member Block.Merge flags then tracknum - 1 else tracknum)
        Block.Collapse

-- | Move selected tracks to the left of the clicked track.
cmd_move_tracks :: Cmd.M m => Msg.Msg -> m ()
cmd_move_tracks msg = do
    (block_id, tracknums, _, _) <- Selection.tracks
    clicked <- Cmd.abort_unless $ clicked_track msg
    dest <- move_tracks block_id tracknums clicked
    -- Shift selection so it's still covering the tracks that moved.
    whenJust (Lists.minimumOn abs $ map (dest-) tracknums) $
        Selection.shift False Selection.Move

move_tracks :: Ui.M m => BlockId -> [TrackNum] -> TrackNum -> m TrackNum
move_tracks block_id sources dest = do
    -- Avoid splitting a track from its merged neighbor.
    dest <- if Just dest > Lists.maximum sources
        then ifM (is_merged_from_right block_id dest)
            (return (dest+1)) (return dest)
        else ifM (is_merged_from_right block_id (dest-1))
            (return (dest-1)) (return dest)
    mapM_ (uncurry (Ui.move_track block_id)) $ if any (<dest) sources
        -- Start at the last source, then insert at the dest counting down.
        then zip (List.sortBy (flip compare) sources) [dest, dest-1 ..]
        -- Start at the first source, then insert at the dest counting up.
        else zip (List.sort sources) [dest ..]
    return dest

-- | True if this has a merged track to its right.  Presumably that track will
-- be collapsed, but I don't check that.
is_merged_from_right :: Ui.M m => BlockId -> TrackNum -> m Bool
is_merged_from_right block_id tracknum = do
    merged <- Block.track_merged <$>
        Ui.get_block_track_at block_id tracknum
    tracknums <- mapMaybeM (Ui.tracknum_of block_id) (Set.toList merged)
    return $ any (== tracknum+1) tracknums
