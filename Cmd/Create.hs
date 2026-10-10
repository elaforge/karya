-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Cmds to create and destroy blocks, views, tracks, and rulers.

    IDs are automatically created from the state_namespace and the other IDs in
    existence.  In general I think it's a bad idea to try to hard to give IDs
    descriptive names because there's nothing keeping them that way.  The
    description should be in the track title.  Even the given numbers will get
    out of date with their position in the block.

    However, I do allow some naming beyond simple numbers for things which are
    unlikely to change, like tempo tracks and rulers, which don't have any
    other title.  And block IDs are used by the sub-derive mechanism, so those
    should be nameable.
-}
module Cmd.Create where
import qualified Control.Monad.State.Strict as Monad.State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import qualified Util.Lists as Lists
import qualified Util.Ranges as Ranges
import qualified Util.Rect as Rect
import qualified Util.Trees as Trees

import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Cmd.Views as Views

import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.GenId as GenId
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Sel as Sel
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Transform as Transform
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.Update as Update

import           Global
import           Types


-- * global modifications

-- | Set the project to the given value and renamespace the old project to the
-- new one.  'Cmd.state_save_file' is not modified, so it will keep saving to
-- the old save file.
renameProject :: Ui.M m => Id.Namespace -> m ()
renameProject ns = do
    oldNs <- Ui.getNamespace
    renamespace oldNs ns
    Ui.setNamespace ns

-- | Rename all IDs in namespace @from@ to @to@.
renamespace :: Ui.M m => Id.Namespace -> Id.Namespace -> m ()
renamespace from to = Transform.mapNamespace $ \ns ->
    if ns == from then to else ns

renameRuler :: Ui.M m => RulerId -> RulerId -> m ()
renameRuler rulerId newName = Transform.mapRulerIds $ \id ->
    if Id.RulerId id == rulerId then Id.unpackId newName else id

-- | Rename multiple RulerIds at once.  This can swap two IDs without
-- colliding.
renameRulers :: Ui.M m => [(RulerId, Id.Id)] -> m ()
renameRulers pairs = Transform.mapRulerIds $ \id ->
    fromMaybe id $ lookup (Id.RulerId id) pairs

renameBlock :: Ui.M m => BlockId -> Id.Id -> m ()
renameBlock blockId newName = Transform.mapBlockIds $ \id ->
    if Id.BlockId id == blockId then newName else id

copyBlock :: Ui.M m => BlockId -> Id.Id -> m ()
copyBlock blockId newName = do
    from <- Ui.getBlock blockId
    void $ Ui.createConfigBlock newName from

-- | Find tracks which are not found in any block.  Probably used to pass them
-- to Ui.destroy_track for \"gc\".
orphanTracks :: Ui.M m => m (Set TrackId)
orphanTracks = do
    blocks <- Ui.gets (Map.elems . Ui.stateBlocks)
    let refTracks = Set.fromList (concatMap Block.blockTrackIds blocks)
    tracks <- Ui.gets (Set.fromAscList . Map.keys . Ui.stateTracks)
    pure $ tracks `Set.difference` refTracks

-- | Like 'orphan_tracks' but more efficiently check if a single track is an
-- orphan.
orphanTrack :: Ui.M m => TrackId -> m Bool
orphanTrack trackId = do
    blocks <- Ui.gets (Map.elems . Ui.stateBlocks)
    pure $ any ((trackId `elem`) . Block.blockTrackIds) blocks

-- | Find rulers which are not found in any block.
orphanRulers :: Ui.M m => m [RulerId]
orphanRulers = do
    blocks <- Ui.gets (Map.elems . Ui.stateBlocks)
    let refRulers = Set.fromList (concatMap Block.blockRulerIds blocks)
    rulers <- Ui.gets (Set.fromAscList . Map.keys . Ui.stateRulers)
    pure $ Set.toList (rulers `Set.difference` refRulers)

-- | Find blocks with no associated views.
orphanBlocks :: Ui.M m => m [BlockId]
orphanBlocks = do
    views <- Ui.gets (Map.elems . Ui.stateViews)
    let refBlocks = Set.fromList (map Block.viewBlock views)
    blocks <- Ui.gets (Set.fromAscList . Map.keys . Ui.stateBlocks)
    pure $ Set.toList (blocks `Set.difference` refBlocks)

-- | Modify track titles with a function.
--
-- TODO this is inadequate.  I need a function to get parsed inst and control
-- track titles separately.  Use TrackTree.track_tree_of to figure inst vs.
-- control.
mapTrackTitles :: Ui.M m => (Text -> Text) -> m ()
mapTrackTitles f = do
    tracks <- Map.assocs . Ui.stateTracks <$> Ui.get
    forM_ tracks $ \(trackId, track) ->
        Ui.setTrackTitle trackId (f (Track.trackTitle track))


-- * block

blockFromTemplate :: Ui.M m => Bool -> BlockId -> m BlockId
blockFromTemplate copyEvents templateId =
    namedBlockFromTemplate copyEvents templateId =<< GenId.blockId Nothing

-- | Create a block which is a copy of another.
namedBlockFromTemplate :: Ui.M m => Bool -- ^ copy the events
    -> BlockId -> Id.Id -> m BlockId
namedBlockFromTemplate copyEvents templateId name = do
    rulerId <- Ui.blockRuler templateId
    blockId <- namedBlock name rulerId
    template <- Ui.getBlock templateId
    Ui.setBlockTitle blockId (Block.blockTitle template)
    let tracks = drop 1 (Block.blockTracks template)
    newTids <- mapMaybeM (addTrack blockId) (zip [1..] tracks)
    -- Copy over the flags and merge status.  I have to merge the analogous
    -- tracks in the new block.
    let oldToNew = Map.fromList $ zip
            (Block.trackIdsOf (map Block.tracklikeId tracks))
            newTids
    forM_ (zip [1..] tracks) $ \(tracknum, btrack) -> do
        Ui.modifyTrackFlags blockId tracknum
            (const (Block.trackFlags btrack))
        Ui.setMergedTracks blockId tracknum $
            Set.fromList $ mapMaybe (`Map.lookup` oldToNew) $ Set.toList $
            Block.trackMerged btrack
    whenM (Ui.hasExplicitSkeleton blockId) $
        Ui.setSkeleton blockId =<< Ui.getSkeleton templateId
    pure blockId

    where
    addTrack blockId (tracknum, btrack) = case Block.tracklikeId btrack of
        Block.TId tid rid -> do
            track <- Ui.getTrack tid
            Just <$> trackEvents blockId rid tracknum
                (Block.trackWidth btrack)
                (Track.track (Track.trackTitle track)
                    (if copyEvents then Track.trackEvents track else mempty))
        _ -> do
            Ui.insertTrack blockId tracknum btrack
            pure Nothing

-- | BlockIds look like @ns\/b1@, @ns\/b2@, etc.
block :: Ui.M m => RulerId -> m BlockId
block = subBlock Nothing

-- | Create a block whose BlockId is prefixed by another: @ns/parent.b1@.
-- The relative block call mechanism supported by the default block call means
-- you can call it from the parent by just writing @.b1@.
subBlock :: Ui.M m => Maybe BlockId -> RulerId -> m BlockId
subBlock maybeParent rulerId = do
    blockId <- GenId.blockId maybeParent
    Ui.createBlock blockId ""
        [Block.track (Block.RId rulerId) Config.rulerWidth]

-- | Create a block with the given ID name.  Useful for blocks meant to be
-- sub-derived.  If the name doesn't contain a @\/@, it gets the current
-- namespace.
namedBlock :: Ui.M m => Id.Id -> RulerId -> m BlockId
namedBlock name rulerId = Ui.createBlock name ""
    [Block.track (Block.RId rulerId) Config.rulerWidth]

-- | Delete a block and any views it appears in.  Also delete any tracks
-- that only appeared in that block.
destroyBlock :: Ui.M m => BlockId -> m ()
destroyBlock blockId = do
    block <- Ui.getBlock blockId
    let trackIds = Block.blockTrackIds block
        rulerIds = Block.blockRulerIds block
    Ui.destroyBlock blockId
    orphans <- orphanTracks
    mapM_ Ui.destroyTrack (filter (`Set.member` orphans) trackIds)
    orphans <- orphanRulers
    mapM_ Ui.destroyRuler (filter (`elem` orphans) rulerIds)

-- * view

-- | Create a view with the default dimensions.
unfittedView :: Ui.M m => BlockId -> m ViewId
unfittedView blockId = do
    (x, y) <- Ui.gets $ findRect Nothing Config.viewSize
        . map Block.viewRect . Map.elems . Ui.stateViews
    let (w, h) = Config.viewSize
    sizedView blockId (Rect.xywh x y w h)

sizedView :: Ui.M m => BlockId -> Rect.Rect -> m ViewId
sizedView blockId rect = do
    viewId <- GenId.viewId blockId
    block <- Ui.getBlock blockId
    viewId <- Ui.createView viewId $
        Block.view block blockId rect Config.zoom
    -- Automatically set the selection on a new view, so it has focus.
    let maybeTracknum = case Block.blockTracks block of
            [] -> Nothing
            [_] -> Just 0
            _ -> Just 1
    whenJust maybeTracknum $ \tracknum ->
        Ui.setSelection viewId Config.insertSelnum $
            Just $ Sel.point tracknum 0 Sel.Positive
            -- TODO I should use the current orientation, but that would make
            -- this and all its callers dependent on Cmd, or need another arg.
    pure viewId

-- | This is like 'unfittedView', but tries to fit the view size to its
-- contents.
--
-- It's in Cmd.M since it needs the screen dimensions.
--
-- Views created during setup are likely to not have the correct height.
-- That's because I haven't received the screen resolution from fltk yet so
-- I make a guess in 'Cmd.get_screen'.
view :: Cmd.M m => BlockId -> m ViewId
view blockId = do
    viewId <- unfittedView blockId
    Views.maximizeAndZoom viewId
    screen <- maybe (Cmd.getScreen Nothing) viewScreen
        =<< Cmd.lookupFocusedView
    rect <- Block.viewRect <$> Ui.getView viewId
    others <- Ui.gets $ filter (\r -> r /= rect && Rect.overlaps r screen)
        . map Block.viewRect . Map.elems . Ui.stateViews
    let (x, y) = findRect (Just screen) (Rect.w rect, Rect.h rect) others
    Ui.setViewRect viewId (Rect.place x y rect)
    pure viewId

-- | Create a view, or focus on it if it already exists.
viewOrFocus :: Cmd.M m => BlockId -> m (Maybe ViewId)
    -- ^ Just ViewId if a new one was created.
viewOrFocus blockId = do
    views <- Ui.viewsOf blockId
    maybe (Just <$> view blockId) (\vid -> Cmd.focus vid *> pure Nothing)
        (Lists.head (Map.keys views))

viewScreen :: Cmd.M m => ViewId -> m Rect.Rect
viewScreen viewId =
    Cmd.getScreen . Just . Rect.upperLeft . Block.viewRect
        =<< Ui.getView viewId

blockView :: Cmd.M m => RulerId -> m ViewId
blockView rulerId = block rulerId >>= view

-- | Destroy a view, along with the underlying block if there were no other
-- views.
destroyView :: Ui.M m => ViewId -> m ()
destroyView viewId = do
    blockId <- Ui.blockIdOf viewId
    Ui.destroyView viewId
    orphans <- orphanBlocks
    when (blockId `elem` orphans) $
        Ui.destroyBlock blockId

-- * track

-- | Create a track and splice it below the current one.  The track will
-- be inserted to the right of the selected track.
spliceBelow :: Cmd.M m => m TrackId
spliceBelow = do
    -- I want to add a track to the right of the selected track.  Taking the
    -- maximum means I should splice after a merged pitch track, if there is
    -- one.
    (blockId, selTracknums, _, _) <- Selection.tracks
    let selTracknum = maximum (1 : selTracknums)
    block <- Ui.getBlock blockId
    let tracknum = trackAfter block selTracknum
    trackId <- focusedTrack tracknum
    whenM (Ui.hasExplicitSkeleton blockId) $
        -- If you create from track 0, it'll be (1, 1) here.
        when (tracknum /= selTracknum) $
            Ui.spliceSkeletonBelow blockId tracknum selTracknum
    pure trackId

spliceAbove :: Cmd.M m => m TrackId
spliceAbove = do
    -- This doesn't need to avoid a merged track like 'splice_below', because
    -- it inserts to the left.
    (blockId, tracknum, _, _) <- Selection.getInsert
    trackId <- focusedTrack tracknum
    whenM (Ui.hasExplicitSkeleton blockId) $
        Ui.spliceSkeletonAbove blockId tracknum (tracknum+1)
    pure trackId

{-
-- | Create a track and make it parent to the current one along with its
-- siblings.  If the selected track has no parent, the new track will become
-- parent to all toplevel tracks and be placed at tracknum 1.  Otherwise, it
-- will be inserted to the right of the parent.
splice_above_all :: Cmd.M m => m TrackId
splice_above_all = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    tree <- Ui.track_tree_of block_id
    (_, parents) <- Cmd.require
        ("splice_above: tracknum not in tree: " ++ show tracknum) $
        Trees.findWithParents ((==tracknum) . num) tree
    let new_tracknum = maybe 1 ((+1) . num . Tree.rootLabel)
            (Lists.head parents)
    let parent = bump . num . Tree.rootLabel <$> Lists.head parents
        bump n = if n >= new_tracknum then n + 1 else n
    track_id <- empty_track block_id new_tracknum
    -- Splice above means splice below the parent!
    case parent of
        Just parent ->
            Ui.splice_skeleton_below block_id new_tracknum parent
        Nothing -> do
            -- No parent?  Becomes the parent of all toplevel tracks.
            let toplevel = map ((+1) . num . Tree.rootLabel) tree
            Ui.add_edges block_id (map ((,) new_tracknum) toplevel)
    return track_id
    where num = Ui.track_tracknum
-}

-- | Get the ancestors (topmost parents) of the selected tracks and create
-- a parent track to them.  It will be inserted to the left of the leftmost
-- ancestor.
spliceAboveAncestors :: Cmd.M m => m TrackId
spliceAboveAncestors = do
    (blockId, tracknums, _, _) <- Selection.tracks
    tree <- TrackTree.trackTreeOf blockId
    let ancestors = Lists.unique $ mapMaybe (ancestor tree) tracknums
    insertAt <- Cmd.require "no selected tracks" $ Lists.minimum ancestors
    trackId <- focusedTrack insertAt
    whenM (Ui.hasExplicitSkeleton blockId) $
        Ui.addEdges blockId (map ((,) insertAt . (+1)) ancestors)
    pure trackId
    where
    ancestor tree tracknum = case List.find find (Trees.flatPaths tree) of
            Nothing -> Nothing
            Just (track, parents, _) ->
                Just $ Ui.trackTracknum $ last (track : parents)
        where find (track, _, _) = Ui.trackTracknum track == tracknum

insertBranch :: Cmd.M m => m ()
insertBranch = do
    (blockId, tracknum, _, _) <- Selection.getInsert
    insertBranchFrom blockId tracknum
    widen =<< Cmd.getFocusedView

-- | Insert tracks using the given one and its children as a template.  The
-- effect is to copy the branch below the selection.
insertBranchFrom :: Cmd.M m => BlockId -> TrackNum -> m ()
insertBranchFrom blockId source = do
    tree <- TrackTree.trackTreeOf blockId
    (track, parents) <- Cmd.require ("not found: "<>pretty (blockId, source)) $
        Trees.findWithParents ((==source) . Ui.trackTracknum) tree
    let right = maximum (Ui.trackTracknum <$> track) + 1
    merged <- Ui.trackMerged blockId source
    appendBelow merged right track
    whenM (Ui.hasExplicitSkeleton blockId) $
        whenJust (Lists.head parents) $ \(Tree.Node parent _) ->
            Ui.addEdges blockId [(Ui.trackTracknum parent, right)]
    where
    -- Starting at tracknum, insert track and its children.
    appendBelow merged tracknum trackNode = do
        forM_ tracks $ \(n, title) -> track blockId n title mempty
        whenM (Ui.hasExplicitSkeleton blockId) $
            Ui.addEdges blockId skel
        -- Technically it's not necessarily merged with its neighbor, but
        -- that's how the usual cmds work, so I'll assume it's true.
        when merged $ Ui.mergeTrack blockId tracknum (tracknum+1)
        where (tracks, skel) = makeTracks tracknum [trackNode]

makeTracks :: TrackNum -> TrackTree.TrackTree
    -> ([(TrackNum, Text)], [(TrackNum, TrackNum)])
makeTracks tracknum tree =
    (concatMap Tree.flatten tracks, Trees.edges (map (fmap fst) tracks))
    where tracks = assignTracknums tracknum tree

-- | Assign ascending tracknums to the given tree in depth-first order.  Return
-- (tracknum, title) pairs.
assignTracknums :: TrackNum -> TrackTree.TrackTree
    -> [Tree.Tree (TrackNum, Text)]
assignTracknums tracknum tree =
    Monad.State.evalState (mapM assign tree) tracknum
    where
    assign (Tree.Node track children) = do
        tracknum <- next
        children <- mapM assign children
        pure $ Tree.Node (tracknum, Ui.trackTitle track) children
        where next = Monad.State.get <* Monad.State.modify (+1)

-- | Insert a track after the selection, or just append one if there isn't one.
-- This is useful for empty blocks which of course have no selection.
insertTrackRight :: Cmd.M m => m TrackId
insertTrackRight = Selection.lookupAnyInsert >>= \case
    Nothing -> appendTrack
    Just (_, (blockId, tracknum, _)) -> do
        block <- Ui.getBlock blockId
        focusedTrack (trackAfter block tracknum)

appendTrack :: Cmd.M m => m TrackId
appendTrack = focusedTrack 99999

focusedTrack :: Cmd.M m => TrackNum -> m TrackId
focusedTrack tracknum = do
    viewId <- Cmd.getFocusedView
    trackAndWiden True viewId tracknum

-- | Add a new track, and widen the view to make sure it's visible.  Give
-- keyboard focus to the title.  If there's a point selection, move it to
-- the new track too.
trackAndWiden :: Ui.M m => Bool -> ViewId -> TrackNum -> m TrackId
trackAndWiden focus viewId tracknum = do
    blockId <- Ui.blockIdOf viewId
    trackId <- track blockId tracknum "" Events.empty
    widen viewId
    tracknum <- clipTracknum blockId tracknum
    when focus $ do
        Ui.damage $ mempty
            { Update._titleFocus = Just (viewId, Just tracknum) }
        whenJustM (Selection.lookupView viewId) $ \sel ->
            if not (Sel.isPoint sel) then pure ()
                else Selection.setView viewId $ Just $
                    Sel.modifyTracks (const tracknum) sel
    pure trackId

-- | Expand the view horizontally to to fit all tracks.
widen :: Ui.M m => ViewId -> m ()
widen viewId = do
    view <- Ui.getView viewId
    embiggened <- Views.contentsRect view
    let rect = Block.viewRect view
    when (Rect.w embiggened > Rect.w rect) $
        Ui.setViewRect viewId $
            Rect.resize (Rect.w embiggened) (Rect.h rect) rect

emptyTrack :: Ui.M m => BlockId -> TrackNum -> m TrackId
emptyTrack blockId tracknum = track blockId tracknum "" Events.empty

-- | Like 'track_events', but copy the ruler from the track to the left.
track :: Ui.M m => BlockId -> TrackNum -> Text -> Events.Events
    -> m TrackId
track blockId tracknum title events = do
    -- Clip to valid range so callers can use an out of range tracknum.
    tracknum <- clipTracknum blockId tracknum
    rulerId <- findRuler (tracknum-1)
    trackEvents blockId rulerId tracknum Config.trackWidth
        (Track.track title events)
    where
    findRuler tracknum
        | tracknum < 0 = pure Ui.noRuler
        | otherwise = maybe (findRuler (tracknum-1)) pure
            =<< Ui.rulerTrackAt blockId tracknum

-- | Lowest level track creator.  The new TrackId will be in the same namespace
-- as the given BlockId.
trackEvents :: Ui.M m =>
    BlockId -> RulerId -> TrackNum -> Types.Width -> Track.Track -> m TrackId
trackEvents blockId rulerId tracknum width track = do
    trackId <- GenId.trackId blockId
    tid <- Ui.createTrack trackId track
    Ui.insertTrack blockId tracknum
        (Block.track (Block.TId tid rulerId) width)
    pure tid

-- | Create a track with the given name, in the same namespace as the BlockId.
namedTrack :: Ui.M m =>
    BlockId -> RulerId -> TrackNum -> Text -> Track.Track -> m TrackId
namedTrack blockId rulerId tracknum name track = do
    ident <- Ui.readId (Id.identName blockId <> "." <> name)
    allTracks <- Ui.gets Ui.stateTracks
    when (Id.TrackId ident `Map.member` allTracks) $
        Ui.throw $ "track " <> showt ident <> " already exists"
    tid <- Ui.createTrack ident track
    Ui.insertTrack blockId tracknum
        (Block.track (Block.TId tid rulerId) Config.trackWidth)
    pure tid

removeSelectedTracks :: Cmd.M m => m ()
removeSelectedTracks = do
    (blockId, tracknums, _, _) <- Selection.tracks
    mapM_ (Ui.removeTrack blockId) (reverse tracknums)

destroySelectedTracks :: Cmd.M m => m ()
destroySelectedTracks = do
    (blockId, tracknums, _, _) <- Selection.tracks
    -- Deleting each track will decrease the tracknum of the ones after it.
    mapM_ (destroyTrack blockId) (zipWith (-) tracknums [0..])

-- | Remove a track from a block.  If that was the only block it appeared in,
-- delete the underlying track.  Rulers are never deleted automatically.
destroyTrack :: Ui.M m => BlockId -> TrackNum -> m ()
destroyTrack blockId tracknum = do
    tracklike <- Ui.require ("invalid tracknum: " <> showt tracknum)
        =<< Ui.trackAt blockId tracknum
    Ui.removeTrack blockId tracknum
    whenJust (Block.trackIdOf tracklike) $ \trackId ->
        whenM (orphanTrack trackId) $
            Ui.destroyTrack trackId

-- | Swap the tracks at the given tracknums.  If one of the tracknums is out
-- of range, the track at the other tracknum will be moved to the beginning or
-- end, i.e. swapped with empty space.
swapTracks :: Ui.M m => BlockId -> TrackNum -> TrackNum -> m ()
swapTracks blockId num0 num1 = do
    track0 <- Ui.blockTrackAt blockId num0
    track1 <- Ui.blockTrackAt blockId num1
    case (track0, track1) of
        (Nothing, Nothing) -> pure ()
        (Just t0, Nothing) -> remove num0 >> insert num1 t0
        (Nothing, Just t1) -> remove num1 >> insert num0 t1
        (Just t0, Just t1) -> do
            remove num0 >> insert num0 t1
            remove num1 >> insert num1 t0
    where
    remove = Ui.removeTrack blockId
    insert = Ui.insertTrack blockId

-- ** util

-- | Clip the tracknum to within the valid range.
clipTracknum :: Ui.M m => BlockId -> TrackNum -> m TrackNum
clipTracknum blockId tracknum = do
    tracks <- Ui.trackCount blockId
    pure $ max 0 (min tracks tracknum)

-- | Get the track to the right of the given tracknum.  This isn't just (+1)
-- because it skips collapsed tracks.
trackAfter :: Block.Block -> TrackNum -> TrackNum
trackAfter block tracknum
    -- It must already be the rightmost tracknum.
    | tracknum == nextTracknum = length (Block.blockTracks block)
    | otherwise = nextTracknum
    where nextTracknum = Ui.skipUnselectableTracks block tracknum 1

-- * ruler

-- | Create a ruler with the given name.
ruler :: Ui.M m => Text -> Ruler.Ruler -> m RulerId
ruler name ruler = do
    ident <- Ui.readId name
    Ui.createRuler ident ruler

-- | Set a block to a new ruler.
newRuler :: Ui.M m => BlockId -> Text -> Ruler.Ruler -> m RulerId
newRuler blockId name r = do
    rulerId <- ruler name r
    setBlockRuler rulerId blockId
    pure rulerId

setBlockRuler :: Ui.M m => RulerId -> BlockId -> m ()
setBlockRuler rulerId blockId =
    Transform.tracks blockId (Block.setRulerId rulerId)

-- * general util

-- | Find a place to fit the given rect.  This is like a tiny window manager.
findRect :: Maybe Rect.Rect -> (Int, Int) -> [Rect.Rect] -> (Int, Int)
findRect maybeScreen (w, _h) rects =
    maybe (0, 0) Rect.upperLeft $ Lists.minimumOn delta holes
    where
    -- First pick holes that fit, by increasing size, then pick the ones
    -- that don't fit, by decreasing size.
    delta rect = (if diff == 0 then -1 else negate (signum diff), abs diff)
        where diff = Rect.w rect - w
    holes = case maybeScreen of
        Nothing -> [Rect.xywh right 0 1 1]
        Just screen -> findHoles rects screen
    right = maximum $ 0 : map Rect.r rects

findHoles :: [Rect.Rect] -> Rect.Rect -> [Rect.Rect]
findHoles rects screen = case Ranges.extract ranges of
    Nothing -> [screen]
    Just rs -> [Rect.xywh x1 (Rect.y screen) (x2-x1) (Rect.h screen)
        | (x1, x2) <- rs]
    where
    extent r = (Rect.x r, Rect.r r)
    ranges = Ranges.invert (extent screen) $ Ranges.ranges $ map extent rects
