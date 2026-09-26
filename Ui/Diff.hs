-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Diff two states to produce a list of Updates, which must be sent to the
    UI to make it display the second state.

    This is unpleasantly complicated and subtle.  I wish I knew a better way!
-}
module Ui.Diff (
    run
    , diff, deriveDiff, trackDiff
    , scoreChanged
    , diffViews
) where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Writer as Writer
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Control as Control
import qualified Util.Logger as Logger
import qualified Util.Maps as Maps
import qualified Util.Ranges as Ranges
import qualified Util.Lists as Lists

import qualified App.Config as Config
import qualified Derive.Deriver.Monad as Derive
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Sel as Sel
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Update as Update

import           Global
import           Types


type DiffM a = Logger.LoggerT (Either Update.UiUpdate Update.DisplayUpdate)
    Identity.Identity a

change :: Update.UiUpdate -> DiffM ()
change = Logger.logs . (:[]) . Left

changes :: [Update.UiUpdate] -> DiffM ()
changes = Logger.logs . map Left

changeDisplay :: Update.DisplayUpdate -> DiffM ()
changeDisplay = Logger.logs . (:[]) . Right

run :: DiffM () -> ([Update.UiUpdate], [Update.DisplayUpdate])
run = Either.partitionEithers . Identity.runIdentity . Logger.exec

-- | Figure out updates needed to turn @st1@ into @st2@.
diff :: Update.UiDamage -> Ui.State -> Ui.State
    -> ([Update.UiUpdate], [Update.DisplayUpdate])
diff damage st1 st2 = postproc damage st2 $ run $ do
    let intersect get keys =
            damaged Maps.zipIntersection st1 st2 get (keys damage)
    diffViews st1 st2 damage
    mapM_ (Control.uncurry3 diffBlock) $
        intersect Ui.stateBlocks Update._blocks
    mapM_ (Control.uncurry3 (diffTrack st2)) $
        intersect Ui.stateTracks (Map.keysSet . Update._tracks)
    -- I don't diff rulers, since they have lots of things in them and rarely
    -- change.  But that means I need the UiDamage hack, and modifications
    -- must be done through Ui.modify_ruler.
    diffState damage st1 st2

-- | Here's where the three different kinds of updates come together.
-- CmdUpdates are converted into UiUpdates, and then all of them converted
-- to DisplayUpdates.
postproc :: Update.UiDamage -> Ui.State
    -> ([Update.UiUpdate], [Update.DisplayUpdate])
    -> ([Update.UiUpdate], [Update.DisplayUpdate])
postproc damage toState (uiUpdates, displayUpdates) =
    (cancelUpdates ui, display ++ refreshSelections toState display)
    where
    ui = Update.toUi damage ++ uiUpdates
    display = displayUpdates ++ toDisplay (mergeUpdates toState ui)
    toDisplay = mapMaybe Update.toDisplay

-- | If the updates have InsertTrack or RemoveTrack the selections may have
-- been moved or deleted.  Emit updates for all selections for all views of
-- blocks with added or removed tracks.
refreshSelections :: Ui.State -> [Update.DisplayUpdate]
    -> [Update.DisplayUpdate]
refreshSelections state updates = concatMap selections viewIds
    where
    viewIds =
        [ viewId | (viewId, view) <- Map.toList (Ui.stateViews state)
        , Set.member (Block.viewBlock view) blockIds
        ]
    blockIds = Set.fromList [blockId |
        Update.Block blockId update <-updates, isTrack update]
    isTrack (Update.RemoveTrack {}) = True
    isTrack (Update.InsertTrack {}) = True
    isTrack _ = False

    selections viewId = fromMaybe [] $ do
        view <- Map.lookup viewId (Ui.stateViews state)
        let update selnum = Update.View viewId $
                Update.Selection selnum
                    (Map.lookup selnum (Block.viewSelections view))
        pure $ map update [0 .. Config.maxSelnums - 1]

-- | DestroyView, DestroyBlock, DestroyTrack, and DestroyRuler cancel out
-- previous updates.
--
-- This isn't technically necessary since callers should be robust against that,
-- but cancelling means less work for them and their warnings are more likely
-- to indicate a real problem.
cancelUpdates :: [Update.UiUpdate] -> [Update.UiUpdate]
cancelUpdates updates = map fst $ filter (not . destroyed) $
    zip updates (drop 1 (List.tails updates))
    where
    destroyed (update, future) = case update of
        Update.View vid view -> case view of
            Update.DestroyView -> False
            _ -> any (== Update.View vid Update.DestroyView) future
        Update.Block bid _ -> futureHas (Update.DestroyBlock bid)
        Update.Track tid _ -> futureHas (Update.DestroyTrack tid)
        Update.Ruler rid -> futureHas (Update.DestroyRuler rid)
        Update.State update -> case update of
            Update.CreateBlock bid _ -> futureHas (Update.DestroyBlock bid)
            Update.CreateTrack tid _ -> futureHas (Update.DestroyTrack tid)
            Update.CreateRuler rid _ -> futureHas (Update.DestroyRuler rid)
            _ -> False
        where futureHas destroy = any (== Update.State destroy) future

-- | Given the track updates, figure out which other tracks have those tracks
-- merged and should also be updated.
--
-- The track diff doesn't generate event updates at all, they are expected to
-- be collected as a side-effect of the event insertion and deletion functions.
-- But that doesn't take into account merged tracks.
mergeUpdates :: Ui.State -> [Update.UiUpdate] -> [Update.UiUpdate]
mergeUpdates state updates = updates ++ concatMap propagate updates
    where
    propagate (Update.Track trackId update)
        | isEventUpdate update =
            map (\tid -> Update.Track tid update) mergesThis
        | otherwise = []
        where mergesThis = Map.findWithDefault [] trackId mergedToTrack
    propagate _ = []
    -- For each track update, find tracks that have it in merged
    trackToMerged = mapMaybe mergedIdsOf
        (concatMap Block.blockTracks (Map.elems (Ui.stateBlocks state)))
    mergedIdsOf track = case Block.tracklikeId track of
        Block.TId trackId _ -> Just (trackId, Block.trackMerged track)
        _ -> Nothing
    -- Map from a track to all tracks that merge it.
    mergedToTrack = Maps.multimap
        [ (mergedId, trackId)
        | (trackId, mergedIds) <- trackToMerged
        , mergedId <- Set.toList mergedIds
        ]
    isEventUpdate (Update.TrackEvents {}) = True
    isEventUpdate Update.TrackAllEvents {} = True
    isEventUpdate _ = False

-- ** view

diffViews :: Ui.State -> Ui.State -> Update.UiDamage -> DiffM ()
diffViews st1 st2 damage =
    mapM_ (uncurry (diffViewPair st1 st2)) $
        damaged Maps.pairs st1 st2 Ui.stateViews (Update._views damage)

diffViewPair :: Ui.State -> Ui.State -> ViewId
    -> Lists.Paired Block.View Block.View -> DiffM ()
diffViewPair st1 st2 viewId = \case
    Lists.Second _ -> change $ Update.View viewId Update.CreateView
    Lists.First _ -> change $ Update.View viewId Update.DestroyView
    Lists.Both view1 view2
        | Block.viewBlock view1 /= Block.viewBlock view2 -> do
            change $ Update.View viewId Update.DestroyView
            change $ Update.View viewId Update.CreateView
        | otherwise -> diffView st1 st2 viewId view1 view2

diffView :: Ui.State -> Ui.State -> ViewId -> Block.View -> Block.View
    -> DiffM ()
diffView st1 st2 viewId view1 view2 = do
    let emit = change . Update.View viewId
    let unequal f = unequalOn f view1 view2
    when (unequal Block.viewRect) $
        emit $ Update.ViewSize (Block.viewRect view2)
    let color = statusColor st2 view2
    when (unequal Block.viewStatus || statusColor st1 view1 /= color) $
        emit $ Update.Status (Block.viewStatus view2) color
    when (unequal Block.viewTrackScroll) $
        emit $ Update.TrackScroll (Block.viewTrackScroll view2)
    when (unequal Block.viewZoom) $
        emit $ Update.Zoom (Block.viewZoom view2)

    -- If the view doesn't have a block I should have failed long before here.
    mapM_ (uncurry (diffSelection emit))
        (Maps.pairs (Block.viewSelections view1) (Block.viewSelections view2))

statusColor :: Ui.State -> Block.View -> Color.Color
statusColor state view =
    case Map.lookup blockId (Ui.stateBlocks state) of
        Just block -> Block.statusColor blockId block
            (UiConfig.configRoot (Ui.stateConfig state))
        Nothing -> Config.statusDefault
    where blockId = Block.viewBlock view

diffSelection :: (Update.View -> DiffM ())
    -> Sel.Num -> Lists.Paired Sel.Selection Sel.Selection -> DiffM ()
diffSelection _ _ (Lists.Both sel1 sel2) | sel1 == sel2 = pure ()
diffSelection emit selnum paired = case paired of
    Lists.Both sel1 sel2
        | sel1 /= sel2 -> emit $ Update.Selection selnum (Just sel2)
    Lists.Second sel2 -> emit $ Update.Selection selnum (Just sel2)
    Lists.First _ -> emit $ Update.Selection selnum Nothing
    _ -> pure ()

-- ** block / track / ruler

diffBlock :: BlockId -> Block.Block -> Block.Block -> DiffM ()
diffBlock blockId block1 block2 = do
    let emit = change . Update.Block blockId
    let unequal f = unequalOn f block1 block2
    when (unequal Block.blockTitle) $
        emit $ Update.BlockTitle (Block.blockTitle block2)
    when (unequal Block.blockConfig) $
        emit $ Update.BlockConfig (Block.blockConfig block2)

    let dtracks1 = Block.blockDisplayTracks block1
        dtracks2 = Block.blockDisplayTracks block2
    let intSkel1 = Block.integrateSkeleton block1
        intSkel2 = Block.integrateSkeleton block2
    when (unequal Block.blockSkeleton || intSkel1 /= intSkel2) $ do
        emit $ Update.BlockSkeleton (Block.blockSkeleton block2) intSkel2
        -- Changing the skeleton may change event styles.
        changes [Update.Track trackId Update.TrackAllEvents
            | trackId <- Block.blockTrackIds block2]

    let btracks1 = Block.blockTracks block1
        btracks2 = Block.blockTracks block2
    let bpairs = Lists.diffIndexOn Block.tracklikeId btracks1 btracks2
    forM_ bpairs $ \(i2, paired) -> case paired of
        Lists.First _ -> emit $ Update.RemoveTrack i2
        Lists.Second track -> emit $ Update.InsertTrack i2 track
        Lists.Both track1 track2 | track1 /= track2 ->
            emit $ Update.BlockTrack i2 track2
        _ -> pure ()

    let dpairs = Lists.diffIndexOn Block.dtracklikeId dtracks1 dtracks2
    forM_ dpairs $ \(i2, paired) -> case paired of
        -- Insert and remove are emitted for UiDamage above, but
        -- the Update.to_display conversion filters them out.
        Lists.First _ -> changeDisplay $
            Update.Block blockId (Update.RemoveTrack i2)
        Lists.Second dtrack -> changeDisplay $
            Update.Block blockId (Update.InsertTrack i2 dtrack)
        Lists.Both dtrack1 dtrack2 | dtrack1 /= dtrack2 -> changeDisplay $
            Update.Block blockId (Update.BlockTrack i2 dtrack2)
        _ -> pure ()

diffTrack :: Ui.State -> TrackId -> Track.Track -> Track.Track -> DiffM ()
diffTrack state trackId track1 track2 = do
    -- Track events updates are collected directly by the Ui.State functions
    -- as they happen.
    let emit = change . Update.Track trackId
    let unequal f = unequalOn f track1 track2
    when (unequal Track.trackTitle) $ do
        emit $ Update.TrackTitle (Track.trackTitle track2)
        -- Changing the title may change the type of the track, which may
        -- change event styles.  If it's going from non-note to note track, it
        -- can also change style of siblings since has_note_children becomes
        -- true.
        changes [Update.Track tid Update.TrackAllEvents
            | tid <- siblingTracks state trackId]

    when (unequal Track.trackBg) $
        emit $ Update.TrackBg (Track.trackBg track2)
    when (unequal Track.trackRender) $
        emit $ Update.TrackRender (Track.trackRender track2)

siblingTracks :: Ui.State -> TrackId -> [TrackId]
siblingTracks state trackId = either (const []) id $ Ui.eval state $ do
    blocks <- Ui.blocksWithTrackId trackId
    pure [tid | (_, tracks) <- blocks, (_, Block.TId tid _) <- tracks]

-- ** state

diffState :: Update.UiDamage -> Ui.State -> Ui.State -> DiffM ()
diffState damage st1 st2 = do
    let emit = change . Update.State
    let pairs get keys = damaged Maps.pairs st1 st2 get (keys damage)
    when (Ui.stateConfig st1 /= Ui.stateConfig st2) $
        emit $ Update.Config (Ui.stateConfig st2)
    forM_ (pairs Ui.stateBlocks Update._blocks) $ \(blockId, paired) ->
        case paired of
            Lists.Second block -> emit $ Update.CreateBlock blockId block
            Lists.First _ -> emit $ Update.DestroyBlock blockId
            _ -> pure ()
    forM_ (pairs Ui.stateTracks (Map.keysSet . Update._tracks)) $
        \(trackId, paired) -> case paired of
            Lists.Second track -> emit $ Update.CreateTrack trackId track
            Lists.First _ -> emit $ Update.DestroyTrack trackId
            _ -> pure ()
    forM_ (pairs Ui.stateRulers Update._rulers) $ \(rulerId, paired) ->
        case paired of
            Lists.Second ruler -> emit $ Update.CreateRuler rulerId ruler
            Lists.First _ -> emit $ Update.DestroyRuler rulerId
            _ -> pure ()

-- * derive diff

type DeriveDiffM a = Writer.WriterT Derive.ScoreDamage Identity.Identity a

runDeriveDiff :: DeriveDiffM () -> Derive.ScoreDamage
runDeriveDiff = snd . Identity.runIdentity . Writer.runWriterT

-- | This diff is meant to determine score damage for the block, which
-- determines what will have to be rederived, if anything.
--
-- This is repeating some work done in 'diff', but is fundamentally different
-- because it cares about nonvisible changes, e.g. track title change on
-- a block without a view.
deriveDiff :: Ui.State -> Ui.State -> Update.UiDamage -> [Update.UiUpdate]
    -> Derive.ScoreDamage
deriveDiff st1 st2 damage updates = postproc $ runDeriveDiff $
    -- If the config has changed, then everything is damaged.
    if unequalOn Ui.stateConfig st1 st2
    then Writer.tell $ mempty
        { Derive.sdamageBlocks =
            Map.keysSet (Ui.stateBlocks st1)
            <> Map.keysSet (Ui.stateBlocks st2)
        }
    else do
        mapM_ (uncurry deriveDiffBlock) $
            damaged Maps.pairs st1 st2 Ui.stateBlocks (Update._blocks damage)
        -- This doesn't check for added or removed tracks, because for them to
        -- have any effect they must be added to or removed from a block, which
        -- 'derive_diff_block' will catch.
        mapM_ (Control.uncurry3 deriveDiffTrack) $
            damaged Maps.zipIntersection st1 st2 Ui.stateTracks
                (Map.keysSet (Update._tracks damage))
    where
    postproc = postprocDamage st2 . (updatesDamage blockRulers updates <>)
    blockRulers = Maps.multimap
        [ (rulerId, blockId)
        | (blockId, block) <- Map.toList (Ui.stateBlocks st2)
        , rulerId <- Block.blockRulerIds block
        ]

-- | Apply a function to only the damaged parts of the state, as expressed
-- by a Set of keys.
damaged :: Ord k => (Map k a -> Map k a -> val) -> state -> state
    -> (state -> Map k a) -> Set k -> val
damaged f st1 st2 get keys =
    f (Map.restrictKeys (get st1) keys) (Map.restrictKeys (get st2) keys)

-- | Fill in 'Derive.sdamage_track_blocks'.
postprocDamage :: Ui.State -> Derive.ScoreDamage -> Derive.ScoreDamage
postprocDamage state (Derive.ScoreDamage tracks _ blocks) =
    Derive.ScoreDamage tracks trackBlocks blocks
    where
    trackBlocks = Set.fromList $ map fst $ Ui.findTracks trackOfBlock
        (Ui.stateBlocks state)
    trackOfBlock (Block.TId tid _) = Map.member tid tracks
    trackOfBlock _ = False

-- | Derive damage from UiUpdates.
updatesDamage :: Map RulerId [BlockId] -> [Update.UiUpdate]
    -> Derive.ScoreDamage
updatesDamage blockRulers updates = mempty
    { Derive.sdamageTracks = tracks
    , Derive.sdamageBlocks = blocks
    }
    where
    tracks = Map.fromListWith (<>) $ mapMaybe Update.trackChanged updates
    blocks = Set.fromList
        [ blockId
        | Update.Ruler rulerId <- updates
        , blockId <- Map.findWithDefault [] rulerId blockRulers
        ]

deriveDiffBlock :: BlockId -> Lists.Paired Block.Block Block.Block
    -> DeriveDiffM ()
deriveDiffBlock blockId = \case
    Lists.Both block1 block2 -> do
        let unequal f = unequalOn f block1 block2
        when (unequal (Text.strip . Block.blockTitle)
                || unequal Block.blockSkeleton)
                -- I could check (Block.config_skeleton . Block.block_config),
                -- but it's indirect, the skeleton change is the direct affect
                -- on derivation.
            blockDamage
        let (ts1, ts2) = (Block.blockTracks block1, Block.blockTracks block2)
        let tpairs = Lists.diffIndexOn Block.tracklikeId ts1 ts2
        forM_ tpairs $ \(_, pair) -> case pair of
            Lists.Both track1 track2
                | flagsDiffer track1 track2 -> blockDamage
                | otherwise -> pure ()
            _ -> blockDamage
    -- This means I wind up with damage on a block that is gone.  I think this
    -- is correct, since the cache will still have recorded dependencies on
    -- that block, which will cause its dependents to be rederived, as
    -- expected.
    Lists.First _ -> blockDamage
    Lists.Second _ -> blockDamage
    where
    blockDamage =
        Writer.tell $ mempty { Derive.sdamageBlocks = Set.singleton blockId }

-- | True if the tracks flags differ in an a way that will require
-- rederivation.
flagsDiffer :: Block.Track -> Block.Track -> Bool
flagsDiffer track1 track2 = relevant track1 /= relevant track2
    where
    relevant = Set.filter mustRederive . Block.trackFlags
    mustRederive flag = case flag of
        -- If this was an uncollapse, I may need a track signal for it now.
        -- Another way to solve this would be to emit track signals for
        -- collapsed tracks, but lots of tracks are collapsed and usually not
        -- expanded, and I'd like derivation to be efficient.
        Block.Collapse -> True
        Block.Merge -> True
        -- These flags are handled by filtering in the player.
        Block.Solo -> False
        Block.Mute -> False
        Block.Disable -> True

deriveDiffTrack :: TrackId -> Track.Track -> Track.Track -> DeriveDiffM ()
deriveDiffTrack trackId track1 track2 =
    when (unequal (Text.strip . Track.trackTitle)
            || unequal Track.trackRender) $
        Writer.tell $ mempty
            { Derive.sdamageTracks = Map.singleton trackId Ranges.everything }
    where
    unequal f = unequalOn f track1 track2

-- * score_changed

-- | This is like 'derive_diff', but it only needs to return a Bool.  It's also
-- more sensitive in that it's looking for any change that you might want to
-- save to disk, not just changes that could require rederivation.
scoreChanged :: Ui.State -> Ui.State -> Update.UiDamage -> Bool
scoreChanged st1 st2 damage = or
    [ Update.isScoreDamage damage
    , unequal Ui.stateConfig
    ]
    where unequal f = unequalOn f st1 st2

-- * events diff

-- | Diff the events on one track.  This will only emit track damage, and won't
-- emit anything if the track title changed, or was created or deleted.  Those
-- diffs should be picked up by the main 'diff'.
trackDiff :: Ui.State -> Ui.State -> TrackId -> Update.UiDamage
trackDiff st1 st2 tid = case (Map.lookup tid t1, Map.lookup tid t2) of
    -- TODO why does it compare track_title here?
    (Just t1, Just t2) | Track.trackTitle t1 == Track.trackTitle t2 -> mempty
        { Update._tracks = Map.singleton tid $
            diffTrackEvents (Track.trackEvents t1) (Track.trackEvents t2)
        }
    _ -> mempty
    where
    t1 = Ui.stateTracks st1
    t2 = Ui.stateTracks st2

diffTrackEvents :: Events.Events -> Events.Events -> Ranges.Ranges TrackTime
diffTrackEvents e1 e2 =
    Ranges.sortedRanges $ mapMaybe diff $
        Lists.pairSortedOn1 Event.start
            (Events.ascending e1) (Events.ascending e2)
    where
    diff (Lists.First e) = Just (Event.range e)
    diff (Lists.Second e) = Just (Event.range e)
    diff (Lists.Both e1 e2)
        | e1 == e2 = Nothing
        | otherwise = Just (Event.start e1, max (Event.end e1) (Event.end e2))


-- * util

unequalOn :: Eq k => (a -> k) -> a -> a -> Bool
unequalOn key a b = key a /= key b
