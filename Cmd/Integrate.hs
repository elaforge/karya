-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Cmd-level support for integration.  These cmds interpret the output of
    the calls in "Derive.Call.Integrate" to create score from deriver output
    and merge it back into the current score.

    An example of track integration:

    - Add \" | <\" to a note track title, which causes damage and a rederive.

    - The integrate call @<@ collects events and puts them into derive results,
    which go into DeriveComplete, which winds up at 'integrate_tracks'.

    - 'integrate_tracks' finds no existing derived tracks, so it merges into
    [], which creates new tracks, and damages the whole block.

    - Then it sets 'Cmd.derive_immediately' on the block, which removes the
    usual derive wait.

    - Derive once again emits integrate results, which winds up at
    'integrate_tracks' again, but since there are no changes this time, there
    is no further damage, and derivation stops.  This additional integration
    just to find out there were no changes is inefficient, but not a big deal
    since it only happens the first time.

    Modify source track:

    - Track damage causes a rederive, which causes the @<@ call to collect
    integrated events.

    - 'integrate_tracks' merges the changes into the destination track (or
    tracks), which damages them.

    - This time when the derive happens, since there was no damage on the
    source track, it gets cached.  The cache intentionally doesn't retain
    integrated events, so @<@ is skipped and I don't get a second derivation.

    Block integration is similar, except that I don't get a double derivation
    when the first new block is created, since the damage is separated to
    a different block.

    It might be a little more orthogonal to omit the thing where
    I automatically create an integrated block or track if there are none, but
    it's convenient in practice.  It does, however, make it tricky to undo
    past the integrate, since if you undo the block\/track creation, the
    integrate call is still there and just creates another.  Be quick!

    This also implements score integration, which is a higher level form of
    integration that simply copies score events directly, without the
    intervening derive step.
-}
module Cmd.Integrate (cmdIntegrate, scoreIntegrate, manualIntegrate) where
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Log as Log

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import qualified Ui.Block as Block
import qualified Ui.Ui as Ui
import qualified Ui.Update as Update

import           Global
import           Types


-- | Derive integrate takes the result of a derivation and merges it into
-- blocks or tracks which are marked as integrate destinations.  A special
-- derive call captures events and saves them in 'Cmd.perf_integrated'.
cmdIntegrate :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmdIntegrate (Msg.DeriveStatus blockId (Msg.DeriveComplete perf _)) = do
    -- If a block or track wants to integrate twice with different events,
    -- I don't know which ones to give to the destinations, and wind up
    -- creating a new track every time.
    let (dups, integrates) = Either.partitionEithers $ map isDup $
            Lists.groupStable Derive.integratedSource
                (Cmd.perfIntegrated perf)
        isDup (x :| xs) = if null xs then Right x else Left x
    unless (null dups) $
        Log.warn $ "these blocks or tracks want to integrate twice: "
            <> Text.intercalate ", "
                (map (either pretty pretty . Derive.integratedSource) dups)
    mapM_ (integrate blockId) integrates
    pure Cmd.Continue
cmdIntegrate _ = pure Cmd.Continue

-- | Integrate the track information into the current state.
integrate :: Cmd.M m => BlockId -> Derive.Integrated -> m ()
integrate derivedBlockId integrated = do
    tracks <- Convert.convert derivedBlockId
        (Derive.integratedEvents integrated)
    case Derive.integratedSource integrated of
        Left blockId -> integrateBlock blockId tracks
        Right trackId -> integrateTracks derivedBlockId trackId tracks

-- | Update and replace the DeriveDestinations for the given TrackId.
-- A source track can have multiple destinations, and each of those is actually
-- a list of DeriveDestinations.
integrateTracks :: Cmd.M m => BlockId -> TrackId -> Convert.Tracks -> m ()
integrateTracks blockId trackId tracks = do
    block <- Ui.getBlock blockId
    -- This means the < call on a non-top block emitted Cmd.perf_integrated.
    unless (trackId `elem` Block.blockTrackIds block) $
        Cmd.throw $ "derivation of " <> pretty blockId <> " wanted to derive "
            <> pretty trackId <> ", which is not in that block"
    itracks <- Block.blockIntegratedTracks <$> Ui.getBlock blockId
    let dests =
            [ dests
            | (sourceId, Block.DeriveDestinations dests) <- itracks
            , sourceId == trackId
            ]
    newDests <- if null dests
        then (:[]) <$> Merge.mergeTracks Merge.KeepTitles blockId tracks []
        else mapM (Merge.mergeTracks Merge.KeepTitles blockId tracks) dests
    unless (null newDests) $
        Log.notice $ "derive track integrate " <> pretty blockId <> " "
            <> pretty trackId <> " to "
            <> pretty (map (map (fst . Block.destNote)) newDests)
    Ui.modifyIntegratedTracks blockId $ replace trackId
        [(trackId, Block.DeriveDestinations dests) | dests <- newDests]
    Cmd.deriveImmediately [blockId]

-- | Look for blocks derived from this one and replace their contents, or
-- create a new block if there are no blocks derived from this one.
integrateBlock :: Cmd.M m => BlockId -> Convert.Tracks -> m ()
integrateBlock sourceId tracks = do
    blocks <- Ui.gets Ui.stateBlocks
    destBlocks <- case integratedFrom blocks of
        [] -> do
            (blockId, dests) <- Merge.createBlock sourceId tracks
            Create.view blockId
            pure [(blockId, dests)]
        integrated -> forM integrated $ \(destId, trackDests) ->
            (,) destId <$> Merge.mergeBlock destId tracks trackDests
    Log.notice $ "derive integrated " <> showt sourceId <> " to "
        <> pretty (map fst destBlocks)
    forM_ destBlocks $ \(destBlockId, trackDests) ->
        Ui.setIntegratedBlock destBlockId $
            Just (sourceId, Block.DeriveDestinations trackDests)
    Cmd.deriveImmediately (map fst destBlocks)
    where
    integratedFrom blocks =
        [ (blockId, dests)
        | (blockId, Just (sourceBlock, Block.DeriveDestinations dests))
            <- map (second Block.blockIntegrated) (Map.toList blocks)
        , sourceBlock == sourceId
        ]


-- * score integrate

-- | For each block with 'Block.ScoreDestinations', figure out if their sources
-- have damage, and if so, re-integrate.
scoreIntegrate :: [Update.UiUpdate] -> Ui.State
    -> Either Ui.Error ([Log.Msg], Ui.State, Update.UiDamage)
scoreIntegrate updates state = Ui.runId state $ do
    -- These both use the passed state instead of using Ui.get when figuring
    -- out if there are updates that require integration.  This way, a
    -- track integrate can't trigger a block integrate, at least not until the
    -- next call to this function.
    trackLogs <- concatMapM scoreIntegrateTracks $
        needsTrackScoreIntegrate updates state
    blockLogs <- mapM scoreIntegrateBlock $
        needsBlockScoreIntegrate updates state
    pure $ map (Log.msg Log.Notice Nothing) (trackLogs ++ blockLogs)

scoreIntegrateBlock :: Ui.M m => BlockId -> m Text
scoreIntegrateBlock sourceId = do
    blocks <- Ui.gets Ui.stateBlocks
    let integrated = integratedFrom blocks
    forM_ integrated $ \(destId, dests) -> do
        dests <- Merge.scoreMergeBlock sourceId destId dests
        Ui.setIntegratedBlock destId $
            Just (sourceId, Block.ScoreDestinations dests)
    pure $ "score integrated " <> showt sourceId <> " to: "
        <> pretty (map fst integrated)
    where
    integratedFrom blocks =
        [ (blockId, dests)
        | (blockId, Just (sourceBlock, Block.ScoreDestinations dests))
            <- map (second Block.blockIntegrated) (Map.toList blocks)
        , sourceBlock == sourceId
        ]

scoreIntegrateTracks :: Ui.M m => (BlockId, TrackId) -> m [Text]
scoreIntegrateTracks (blockId, trackId) = do
    itracks <- Block.blockIntegratedTracks <$> Ui.getBlock blockId
    let dests =
            [ dests
            | (sourceId, Block.ScoreDestinations dests) <- itracks
            , sourceId == trackId
            ]
    newDests <- mapM (Merge.scoreMergeTracks blockId trackId) dests
    Ui.modifyIntegratedTracks blockId $ replace trackId
        [(trackId, Block.ScoreDestinations dests) | dests <- newDests]
    pure $ map msg newDests
    where
    msg dests = "score integrated " <> showt trackId <> ": "
        <> Text.intercalate ", "
            [ pretty sourceId <> " -> " <> pretty destId
            | (sourceId, (destId, _)) <- dests
            ]

replace :: Eq key => key -> [(key, a)] -> [(key, a)] -> [(key, a)]
replace key new xs = new ++ filter ((/=key) . fst) xs

-- | Blocks which are block score integrate sources and have damage.
needsBlockScoreIntegrate :: [Update.UiUpdate] -> Ui.State -> [BlockId]
needsBlockScoreIntegrate updates state =
    filter hasIntegrated $ Map.keys $ flip Map.restrictKeys damagedBlocks $
        Ui.stateBlocks state
    where
    -- TODO this is a linear search through all blocks
    hasIntegrated blockId = not $ null
        [ ()
        | Just (destBlockId, Block.ScoreDestinations {}) <-
            map Block.blockIntegrated $ Map.elems (Ui.stateBlocks state)
        , blockId == destBlockId
        ]
    damagedBlocks = Set.fromList $ mapMaybe blockChanged updates
    blockChanged (Update.Block bid _) = Just bid
    blockChanged _ = Nothing

-- | Tracks which are track score integrate sources and have damage.
needsTrackScoreIntegrate :: [Update.UiUpdate] -> Ui.State
    -> [(BlockId, TrackId)]
needsTrackScoreIntegrate updates state = Lists.unique $
    concatMap (integratedBlocks . fst) $ mapMaybe Update.trackChanged updates
    where
    integratedBlocks trackId =
        [ (blockId, trackId) | (blockId, block) <- blocksWith trackId
        , hasIntegrated block trackId
        ]
    -- TODO this is a linear search through all blocks, as is
    -- Ui.blocks_with_track_id.
    blocksWith trackId = filter (hasTrack trackId . snd) $ Map.toList $
        Ui.stateBlocks state
    hasTrack trackId block = trackId `elem` Block.blockTrackIds block
    hasIntegrated block trackId = not $ null
        [ ()
        | (sourceTrackId, Block.ScoreDestinations {})
            <- Block.blockIntegratedTracks block
        , trackId == sourceTrackId
        ]

-- * manual integrate

-- | Find blocks with the source key, and merge the given tracks into them.
--
-- If you are creating a new track, you need to have already done that and put
-- an empty destination in it.  Otherwise, this will find no existing
-- destinations and do nothing.
manualIntegrate :: Ui.M m => Block.SourceKey -> Convert.Track -- ^ note track
    -> [Convert.Track] -- ^ dependent control tracks
    -> m ()
manualIntegrate key note controls = do
    blockDests <- manualDestinations key . Map.toList <$>
        Ui.gets Ui.stateBlocks
    forM_ blockDests $ \(blockId, dests) -> do
        newDests <- forM dests $ \dest ->
            Merge.mergeTracks Merge.KeepTitles blockId [(note, controls)]
                [dest]
        Ui.setIntegratedManual blockId key (Just (concat newDests))

-- | Find all manual derive destinations with the given key.
manualDestinations :: Block.SourceKey -> [(a, Block.Block)]
    -> [(a, [Block.NoteDestination])]
manualDestinations key = filter (not . null . snd)
    . map (second (Map.findWithDefault [] key . Block.blockIntegratedManual))
