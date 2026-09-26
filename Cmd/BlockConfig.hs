-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds that affect global block config but don't fit into any of the
-- more specefic modules.
module Cmd.BlockConfig (
    cmdToggleEdge
    , toggleMergeAll
    , toggleMergeSelected
    , cmdOpenBlock
    , cmdAddBlockTitle
    , clip
    , collapseChildren
    , expandChildren
    , append
    , cmdToggleFlag
    , toggleCollapseEmpty
    , cmdSetSolo
    , cmdMuteOrUnsolo
    , cmdExpandTrack
    , cmdMoveTracks
    , moveTracks
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
cmdToggleEdge :: Cmd.M m => Msg.Msg -> m ()
cmdToggleEdge msg = do
    (blockId, selTracknum, _, _) <- Selection.getInsert
    clickedTracknum <- Cmd.abortUnless $ clickedTrack msg
    -- The click order goes in the arrow direction, parent to child.
    let edge = (selTracknum, clickedTracknum)
    success <- Ui.toggleSkeletonEdge False blockId edge
    unless success $
        Log.warn $ "refused to add cycle-creating edge: " <> showt edge
    -- The shift below is incorrect.  Anyway, a common case is to splice
    -- a track above and then delete the unwanted edges, and moving the
    -- selection makes that inconvenient.
    -- let shift = clicked_tracknum - sel_tracknum
    -- if success
    --     then Selection.cmd_shift_selection Config.insert_selnum shift False
    --     else Log.warn $ "refused to add cycle-creating edge: " ++ show edge

clickedTrack :: Msg.Msg -> Maybe TrackNum
clickedTrack msg = case (Msg.mouseDown msg, Msg.contextTrack msg) of
    (True, Just (tracknum, _)) -> Just tracknum
    _ -> Nothing

-- * merge

-- | Merge all adjacent note/pitch pairs.  If they're already all merged,
-- unmerge them all.
toggleMergeAll :: Ui.M m => BlockId -> m ()
toggleMergeAll blockId = toggleMerge blockId =<< Info.blockTracks blockId

toggleMergeSelected :: Cmd.M m => m ()
toggleMergeSelected = do
    (blockId, tracknums) <- Selection.tracknums
    tracks <- Info.blockTracks blockId
    toggleMerge blockId $ filter
        ((`elem` tracknums) . Ui.trackTracknum . Info.trackInfo) tracks

toggleMerge :: Ui.M m => BlockId -> [Info.Track] -> m ()
toggleMerge blockId tracks = do
    tracknums <- mergeable blockId $ noParents
        [ Ui.trackTracknum note
        | Info.Track note (Info.Note {}) <- tracks
        ]
    ifM (allM (Ui.trackMerged blockId) tracknums)
        (mapM_ (Ui.unmergeTrack blockId) tracknums)
        (mapM_ (\t -> Ui.mergeTrack blockId t (t+1)) tracknums)
    where
    -- A note track parent can't merge, so don't count it.
    noParents (t1:t2:ts) | t1 + 1 == t2 = noParents (t2:ts)
    noParents (t:ts) = t : noParents ts
    noParents [] = []

mergeable :: Ui.M m => BlockId -> [TrackNum] -> m [TrackNum]
mergeable blockId =
    filterM $ \tracknum -> isControlOrPitch tracknum blockId (tracknum + 1)

-- | True if the track is a control or pitch track, and a child of the given
-- tracknum.
isControlOrPitch :: Ui.M m => TrackNum -> BlockId -> TrackNum -> m Bool
isControlOrPitch parent blockId tracknum =
    Ui.eventTrackAt blockId tracknum >>= \case
        Nothing -> pure False
        Just trackId -> andM
            [ (`elem` [ParseTitle.ControlTrack, ParseTitle.PitchTrack]) <$>
                ParseTitle.trackType <$> Ui.getTrackTitle trackId
            , TrackTree.isChildOf blockId parent tracknum
            ]

-- * misc

cmdOpenBlock :: Cmd.M m => Bool -> m ()
cmdOpenBlock alignNewView = do
    sel <- Selection.events
    blockId <- Cmd.getFocusedBlock
    parent <- Ui.getView =<< Cmd.getFocusedView
    let blockCalls = NoteTrackParse.exprBlockCalls True blockId . Event.text
    forM_ sel $ \(_, events) -> forM_ events $ \event ->
        mapM_ (open parent event) =<< blockCalls event
    where
    open parent event blockId = do
        newView <- Create.viewOrFocus blockId
        when alignNewView $ whenJust newView $ \viewId ->
            alignViewTo parent (Event.start event) (Event.end event) viewId

-- | Line the ViewId up to be right next to the given parent view and fit into
-- the given time range.
alignViewTo :: Cmd.M m => Block.View -> TrackTime -> TrackTime -> ViewId
    -> m ()
alignViewTo parent start end viewId = do
    let x = Rect.r $ Block.viewRect parent
        top = Block.screenPixels parent start
        bottom = Block.screenPixels parent end
    width <- Rect.w . Block.trackRect <$> Ui.getView viewId
    Views.setTrackRect viewId $ Rect.xywh x top width (bottom - top)
    Views.zoomToRuler viewId

-- | Add an empty title and focus on it.  The text widget hides itself when
-- empty, so the space gets it to appear for focus.
cmdAddBlockTitle :: Cmd.M m => Msg.Msg -> m ()
cmdAddBlockTitle _ = do
    viewId <- Cmd.getFocusedView
    blockId <- Block.viewBlock <$> Ui.getView viewId
    title <- Ui.getBlockTitle blockId
    when (Text.null title) $
        Ui.setBlockTitle blockId " "
    Ui.damage $ mempty { Update._titleFocus = Just (viewId, Nothing) }

-- | Clip a block to the selection.
clip :: Cmd.M m => m ()
clip = do
    (blockId, _, _, pos) <- Selection.getInsert
    clipTo blockId pos

clipTo :: Ui.M m => BlockId -> TrackTime -> m ()
clipTo blockId end = do
    RulerUtil.localMeter RulerUtil.Block blockId $
        Meter.modifySections (Meter.sectionsTake end)
    trackIds <- Ui.trackIdsOf blockId
    forM_ trackIds $ \trackId ->
        Ui.modifyEvents trackId $ Events.clip True end

-- * collapse / expand tracks

-- | Collapse all the children of this track.
collapseChildren :: Ui.M m => BlockId -> TrackId -> m ()
collapseChildren blockId trackId = do
    children <- TrackTree.getChildrenOf blockId trackId
    forM_ children $ \track -> Ui.addTrackFlag
        blockId (Ui.trackTracknum track) Block.Collapse

-- | Expand all collapsed children of this track.  Tracks that were merged
-- when they were collapsed will be left merged.
expandChildren :: Ui.M m => BlockId -> TrackId -> m ()
expandChildren blockId trackId = do
    children <- TrackTree.getChildrenOf blockId trackId
    merged <- mconcatMap Block.trackMerged . Block.blockTracks
        <$> Ui.getBlock blockId
    forM_ children $ \track ->
        when (Set.member (Ui.trackId track) merged) $
            Ui.removeTrackFlag
                blockId (Ui.trackTracknum track) Block.Collapse

-- * merge blocks

append :: Ui.M m => BlockId -> BlockId -> m ()
append dest source = do
    -- By convention the first track is just a ruler.
    tracks <- drop 1 . Block.blockTracks <$> Ui.getBlock source
    tracknum <- Ui.trackCount dest
    tracknum <- if tracknum <= 1 then pure tracknum else do
        Ui.insertTrack dest tracknum Block.divider
        pure (tracknum + 1)
    forM_ (zip [tracknum..] tracks) $ \(i, track) ->
        Ui.insertTrack dest i track
    skel <- Ui.getSkeleton dest
    edges <- Skeleton.flatten <$> Ui.getSkeleton source
    let offset = tracknum - 1 -- -1 because I dropped the first track.
    skel <- Ui.require "couldn't add edges to skel" $
        Skeleton.addEdges [(s+offset, e+offset) | (s, e) <- edges] skel
    Ui.setSkeleton dest skel

-- * track

-- | If the flag is set on any of the selected tracks, unset it.  Otherwise,
-- set it.  This is a bit more complicated than a simple toggle because if
-- you have a collapsed track where one is soloed and one isn't, a simple
-- toggle would just move the solo flag from one track to the other, leaving
-- the track as a whole soloed.
cmdToggleFlag :: Cmd.M m => Block.TrackFlag -> m ()
cmdToggleFlag flag = do
    (blockId, tracknums, _, _) <- Selection.tracks
    flags <- mapM (Ui.trackFlags blockId) tracknums
    let set = any (Set.member flag) flags
    forM_ tracknums $ \tracknum -> if set
        then Ui.removeTrackFlag blockId tracknum flag
        else Ui.addTrackFlag blockId tracknum flag

-- | Collapse all tracks that are empty and have all empty children.  If
-- everything empty is collapsed, expand them all.  Don't consider
-- merge-collapsed.
toggleCollapseEmpty :: Ui.M m => BlockId -> m ()
toggleCollapseEmpty blockId = do
    tracks <- TrackTree.trackTreeOf blockId
    tracks <- traverse (traverse (\t -> (t,) <$> isEmpty t)) tracks
    let empty = concatMap emptyTracks tracks
    allCollapsed <- allM isCollapsed empty
    forM_ (map Ui.trackTracknum empty) $ \tracknum -> if allCollapsed
        then Ui.removeTrackFlag blockId tracknum Block.Collapse
        else Ui.addTrackFlag blockId tracknum Block.Collapse
    where
    isEmpty = fmap Events.null . Ui.getEvents . Ui.trackId
    isCollapsed = fmap Block.isCollapsed . Ui.trackFlags blockId
        . Ui.trackTracknum
    emptyTracks (Tree.Node (track, empty) children)
        | empty && all snd leaves = track : map fst leaves
        | otherwise = concatMap emptyTracks children
        where leaves = concatMap Trees.leaves children

-- | Enable Solo on the track and disable Mute.  It's bound to a double click
-- so when this cmd fires I have to do undo the results of the single click.
-- Perhaps mute and solo should be exclusive in general.
cmdSetSolo :: Cmd.M m => Msg.Msg -> m ()
cmdSetSolo msg = do
    tracknum <- Cmd.abortUnless $ clickedTrack msg
    blockId <- Cmd.getFocusedBlock
    Ui.removeTrackFlag blockId tracknum Block.Mute
    Ui.toggleTrackFlag blockId tracknum Block.Solo

-- | Unset solo if it's set, otherwise toggle the mute flag.
cmdMuteOrUnsolo :: Cmd.M m => Msg.Msg -> m ()
cmdMuteOrUnsolo msg = do
    blockId <- Cmd.getFocusedBlock
    tracknum <- Cmd.abortUnless $ clickedTrack msg
    flags <- Ui.trackFlags blockId tracknum
    if Block.Solo `Set.member` flags
        then Ui.removeTrackFlag blockId tracknum Block.Solo
        else Ui.toggleTrackFlag blockId tracknum Block.Mute

cmdExpandTrack :: Cmd.M m => Msg.Msg -> m ()
cmdExpandTrack msg = do
    blockId <- Cmd.getFocusedBlock
    tracknum <- Cmd.abortUnless $ clickedTrack msg
    expandTrack blockId tracknum

-- | Expand collapsed track.  If it's Merged, try to expand the one to the
-- left.  If a track with a merged is collapsed, this let's me click on either
-- to expand.
expandTrack :: Ui.M m => BlockId -> TrackNum -> m ()
expandTrack blockId tracknum = do
    flags <- Ui.trackFlags blockId tracknum
    Ui.removeTrackFlag blockId
        (if Set.member Block.Merge flags then tracknum - 1 else tracknum)
        Block.Collapse

-- | Move selected tracks to the left of the clicked track.
cmdMoveTracks :: Cmd.M m => Msg.Msg -> m ()
cmdMoveTracks msg = do
    (blockId, tracknums, _, _) <- Selection.tracks
    clicked <- Cmd.abortUnless $ clickedTrack msg
    dest <- moveTracks blockId tracknums clicked
    -- Shift selection so it's still covering the tracks that moved.
    whenJust (Lists.minimumOn abs $ map (dest-) tracknums) $
        Selection.shift False Selection.Move

moveTracks :: Ui.M m => BlockId -> [TrackNum] -> TrackNum -> m TrackNum
moveTracks blockId sources dest = do
    -- Avoid splitting a track from its merged neighbor.
    dest <- if Just dest > Lists.maximum sources
        then ifM (isMergedFromRight blockId dest)
            (pure (dest+1)) (pure dest)
        else ifM (isMergedFromRight blockId (dest-1))
            (pure (dest-1)) (pure dest)
    mapM_ (uncurry (Ui.moveTrack blockId)) $ if any (<dest) sources
        -- Start at the last source, then insert at the dest counting down.
        then zip (List.sortBy (flip compare) sources) [dest, dest-1 ..]
        -- Start at the first source, then insert at the dest counting up.
        else zip (List.sort sources) [dest ..]
    pure dest

-- | True if this has a merged track to its right.  Presumably that track will
-- be collapsed, but I don't check that.
isMergedFromRight :: Ui.M m => BlockId -> TrackNum -> m Bool
isMergedFromRight blockId tracknum = do
    merged <- Block.trackMerged <$>
        Ui.getBlockTrackAt blockId tracknum
    tracknums <- mapMaybeM (Ui.tracknumOf blockId) (Set.toList merged)
    pure $ any (== tracknum+1) tracknums
