-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds to do with \"refactoring\".  This basically means fancy
-- copy-paste-like operations.
module Cmd.Factor where
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Num as Num

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Selection as Selection

import qualified Derive.Eval as Eval
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ruler as Ruler
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Transform as Transform
import qualified Ui.Ui as Ui

import           Global
import           Types


-- | Split the block at the time of the current selection, and create a new
-- block and view with everything afterwards.  The old and new blocks are
-- renamed with @-1@ and @-2@ suffixes, respectively.  The old block is renamed
-- for symmetry with the new one, but mostly because it's changed duration, so
-- previous calls are probably no longer valid.
splitTime :: Cmd.M m => m BlockId -- ^ BlockId of new block
splitTime = do
    (blockId, _, _, pos) <- Selection.getInsert
    let (fromBlock, toBlock) = splitNames blockId
    toBlockId <- splitTimeAt blockId pos toBlock
    Create.view toBlockId
    Create.renameBlock blockId fromBlock
    pure toBlockId

-- | Create a new block from template, then copy over all the events below the
-- given time.  Clear the source track, and trim events that overlap the split
-- point.  Modify the ruler (locally!) in the old and new blocks.
splitTimeAt :: Ui.M m => BlockId -> ScoreTime -> Id.Id -> m BlockId
splitTimeAt fromBlockId pos blockName = do
    tracks <- Ui.tracknumsOf fromBlockId
    -- Copy over the new events.
    trackEvents <- forM tracks $ \(trackId, tracknum) -> do
        events <- Events.atAfter pos <$> Ui.getEvents trackId
        let shifted = map (Event.start_ %= subtract pos) events
        pure (tracknum, shifted)
    -- Trim the old events.
    forM_ tracks $ \(trackId, _) -> do
        events <- fst . Events.split pos <$> Ui.getEvents trackId
        Ui.modifyEvents trackId $ const $ Events.clip False pos events
    -- Create new block.
    toBlockId <- Create.namedBlockFromTemplate False fromBlockId
        blockName
    forM_ trackEvents $ \(tracknum, events) -> do
        trackId <- Ui.getEventTrackAt toBlockId tracknum
        Ui.insertEvents trackId events
    -- Trim rulers on each.
    let dur = Meter.timeToDuration pos
    localSections fromBlockId $ RulerUtil.extract 0 dur
    localSections toBlockId $ RulerUtil.delete 0 dur
    pure toBlockId

splitNames :: BlockId -> (Id.Id, Id.Id)
splitNames blockId =
    (Id.modifyName (<>"-1") id, Id.modifyName (<>"-2") id)
    where id = Id.unpackId blockId

-- | Put all tracks with a after the selection into a new block.
--
-- Unlike 'split_time' I don't rename the source block, because the length
-- is unchanged.
splitTrack :: Cmd.M m => m BlockId
splitTrack = do
    (blockId, tracknum, _, _) <- Selection.getInsert
    toBlockId <- splitTrackAt blockId tracknum
        (snd (splitNames blockId))
    Create.view toBlockId
    pure toBlockId

splitTrackAt :: Ui.M m => BlockId -> TrackNum -> Id.Id -> m BlockId
splitTrackAt fromBlockId splitAt blockName = do
    toBlockId <- Create.namedBlock blockName =<< Ui.rulerOf fromBlockId
    -- Move tracks.
    tracks <- zip [0..] . Block.blockTracks <$> Ui.getBlock fromBlockId
    forM_ (dropWhile ((<splitAt) . fst) tracks) $ \(tracknum, track) ->
        Ui.insertTrack toBlockId (tracknum - splitAt + 1) track
    forM_ (takeWhile ((>=splitAt) . fst) (reverse tracks)) $
        \(tracknum, _) -> Ui.removeTrack fromBlockId tracknum
    -- Copy over the skeleton.
    whenM (Ui.hasExplicitSkeleton toBlockId) $ do
        skeleton <- Ui.getSkeleton fromBlockId
        Ui.setSkeleton toBlockId $ Skeleton.make
            [ (from-splitAt + 1, to-splitAt + 1)
            | (from, to) <- Skeleton.flatten skeleton
            , from >= splitAt && to >= splitAt
            ]
    pure toBlockId

-- | Copy the selection into a new block, and replace it with a call to that
-- block.
selection :: Cmd.M m => Id.Id -> m BlockId
selection name = do
    blockId <- selection_ True False name
    Create.view blockId
    pure blockId

-- | Copy the selection to a relative block, and replace it with a relative
-- block call.
selectionRelative :: Cmd.M m => Id.Id -> m BlockId
selectionRelative name = do
    blockId <- selection_ True True name
    Create.view blockId
    pure blockId

-- | Same as 'selection_relative' because I always type the wrong one.
relativeSelection :: Cmd.M m => Id.Id -> m BlockId
relativeSelection = selectionRelative

-- | Create a number of alternate versions of the selection, and insert
-- an @alt@ call.
selectionAlts :: Cmd.M m => Bool -> Int -> Id.Id -> m [BlockId]
selectionAlts relative alts name
    | alts <= 0 = pure []
    | otherwise = do
        alt1 <- selection_ True relative $ Id.modifyName (<>"1") name
        (blockId, _, trackIds, range) <- Selection.tracks
        altn <- forM [2..alts] $ \n ->
            Create.namedBlockFromTemplate True alt1 $
                altName blockId (Id.modifyName (<> showt n) name)
        let alts = alt1 : altn
        mapM_ Create.view alts
        let call = Text.unwords $
                "alt" : map (Eval.blockIdToCall relative blockId) alts
        let (start, end) = Events.rangeTimes range
        whenJust (Lists.head trackIds) $ \trackId ->
            Ui.insertEvent trackId $ Event.event start (end-start) call
        pure alts
    where
    altName blockId name =
        if relative then makeRelative blockId name else name

-- | Copy the selection into a new block, and replace it with a call to that
-- block.
selection_ :: Cmd.M m => Bool
    -- ^ replace the copied events with a call to the new block
    -> Bool -- ^ create relative block call
    -> Id.Id -> m BlockId
selection_ replace relative name = do
    (blockId, tracknums, trackIds, range) <- Selection.tracks
    name <- pure $ if relative
        then makeRelative blockId name else name
    toBlockId <- extract name blockId tracknums trackIds range
    when replace $
        replaceWithCall blockId trackIds range toBlockId relative
    pure toBlockId

makeRelative :: BlockId -> Id.Id -> Id.Id
makeRelative caller name =
    Id.setName (Eval.makeRelative caller (Id.idName name)) name

-- | Copy the ranges to a new block with the given Id.
extract :: Ui.M m => Id.Id -> BlockId -> [TrackNum] -> [TrackId]
    -> Events.Range -> m BlockId
extract name blockId tracknums trackIds range = do
    rulerId <- Ui.blockRuler blockId
    toBlockId <- Create.namedBlock name rulerId
    let (start, end) = Events.rangeTimes range
    forM_ (zip [1..] trackIds) $ \(tracknum, trackId) -> do
        title <- Ui.getTrackTitle trackId
        events <- Events.inRange range <$> Ui.getEvents trackId
        -- Shift the events back to start at 0.
        Create.track toBlockId tracknum title $
            Events.mapEvents (Event.start_ %= subtract start) events
    clippedSkeleton blockId toBlockId tracknums
    -- It's easier to create all the tracks and then delete the empty ones.
    -- If I tried to just not create those tracks then 'clipped_skeleton' would
    -- have to get more complicated.
    deleteEmptyTracks toBlockId
    -- Create a clipped ruler.
    localSections toBlockId $
        RulerUtil.extract (Meter.timeToDuration start)
            (Meter.timeToDuration end)
    pure toBlockId

-- | Clear selected range and put in a call to the new block.
replaceWithCall :: Ui.M m => BlockId -> [TrackId] -> Events.Range -> BlockId
    -> Bool -> m ()
replaceWithCall blockId trackIds range toBlockId relative = do
    Edit.clearRange trackIds range
    let (start, end) = Events.rangeTimes range
    whenJust (Lists.head trackIds) $ \trackId ->
        Ui.insertEvent trackId $ Event.event start (end-start)
            (Eval.blockIdToCall relative blockId toBlockId)

-- ** relative calls

-- | Rename all blocks with the old parent as a prefix.  Unlike
-- 'rebase_relative_calls', this doesn't modify any events.
rebaseIds :: Ui.M m => BlockId -> BlockId -> m ()
rebaseIds oldParent newParent = Transform.mapBlockIds $ \id ->
    case rebaseId oldParent newParent (Id.BlockId id) of
        Nothing -> id
        Just newId -> newId

-- | Move a relative callee from one parent to another, or Nothing if it's
-- not a child of that parent.
rebaseId :: BlockId -> BlockId -> BlockId -> Maybe Id.Id
rebaseId oldParent newParent child = case Eval.parseRelativeId child of
    Just (parent, name)
        | parent == oldParent -> Just $ makeRelative newParent name
        | otherwise -> Nothing
    Nothing -> Nothing

-- | If there's a point selection, create a new empty block based on the
-- current one.  If the selection has time, then the new block will have only
-- the selected tracks with a ruler clipped to the selected range.
blockFromTemplate :: Cmd.M m => m ()
blockFromTemplate = do
    sel <- Selection.get
    if Sel.isPoint sel
        then void $ Create.view =<< Create.blockFromTemplate False
            =<< Cmd.getFocusedBlock
        else void blockTemplateFromSelection

deleteEmptyTracks :: Ui.M m => BlockId -> m ()
deleteEmptyTracks blockId = do
    trackIds <- filterM (fmap Events.null . Ui.getEvents)
        =<< Ui.trackIdsOf blockId
    mapM_ Ui.destroyTrack trackIds

-- * named block

blockTemplateFromSelection :: Cmd.M m => m BlockId
blockTemplateFromSelection =
    Selection.tracks >>= \(blockId, _, trackIds, range) -> do
        toBlockId <- blockTemplate blockId trackIds range
        Create.view toBlockId
        pure toBlockId

-- | Create a new block with the given tracks and ruler clipped to the given
-- range.
blockTemplate :: Ui.M m => BlockId -> [TrackId] -> Events.Range -> m BlockId
blockTemplate blockId trackIds range = do
    toBlockId <- Create.block =<< Ui.blockRuler blockId
    forM_ (zip [1..] trackIds) $ \(tracknum, trackId) -> do
        title <- Ui.getTrackTitle trackId
        Create.track toBlockId tracknum title mempty
    -- Create skeleton.
    clippedSkeleton blockId toBlockId
        =<< mapM (Ui.getTracknumOf blockId) trackIds
    -- Create a clipped ruler.
    let (start, end) = Events.rangeTimes range
    localSections toBlockId $
        RulerUtil.extract (Meter.timeToDuration start)
            (Meter.timeToDuration end)
    pure toBlockId

clippedSkeleton :: Ui.M m => BlockId -> BlockId -> [TrackNum] -> m ()
clippedSkeleton fromBlock toBlock tracknums =
    whenM (Ui.hasExplicitSkeleton toBlock) $
        case (Lists.minimum tracknums, Lists.maximum tracknums) of
            (Just low, Just high) -> do
                edges <- Skeleton.flatten <$> Ui.getSkeleton fromBlock
                Ui.setSkeleton toBlock $ Skeleton.make
                    [ (from-low + 1, to-low + 1) | (from, to) <- edges
                    , Num.inRange low (high+1) from, Num.inRange low (high+1) to
                    ]
            _ -> pure ()

-- * order block

-- | Create a new block containing calls to the given BlockIds.
orderBlock :: Cmd.M m => Id.Id -> [BlockId] -> m BlockId
orderBlock name blockIds = do
    blockId <- Create.namedBlock name Ui.noRuler
    orderTrack blockId blockIds
    Create.view blockId
    pure blockId

-- | Append a track to the given block with calls to the given BlockIds.  The
-- calling track will have a 1:1 time relationship with the calls, which is
-- useful for lilypond derivation since it only understands 1:1.  Also
-- modify the ruler to be the concatenation of the rulers of the sub-blocks.
orderTrack :: Ui.M m => BlockId -> [BlockId] -> m TrackId
orderTrack blockId subBlocks = do
    rulerIds <- mapM Ui.rulerOf subBlocks
    meters <- mapM RulerUtil.getMeter rulerIds
    let durs = map Meter.meterEnd meters
        starts = scanl (+) 0 durs
        events =
            [ Event.event start dur (blockIdToCall blockId)
            | (start, dur, blockId) <- zip3 starts durs subBlocks
            ]
    localBlock blockId $ const $ mconcat meters
    Create.track blockId 9999 ">" (Events.fromList events)

blockIdToCall :: BlockId -> Text
blockIdToCall = Id.identName

-- * util

localSections :: Ui.M m => BlockId -> ([Meter.MSection] -> [Meter.MSection])
    -> m ()
localSections blockId = localBlock blockId . Meter.modifySections

localBlock :: Ui.M m => BlockId -> (Meter.Meter -> Meter.Meter) -> m ()
localBlock blockId modify = do
    RulerUtil.localBlock blockId (Right . Ruler.modifyMeter modify)
    pure ()
