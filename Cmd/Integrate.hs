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
module Cmd.Integrate (cmd_integrate, score_integrate, manual_integrate) where
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Seq as Seq
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
cmd_integrate :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmd_integrate (Msg.DeriveStatus block_id (Msg.DeriveComplete perf _)) = do
    -- If a block or track wants to integrate twice with different events,
    -- I don't know which ones to give to the destinations, and wind up
    -- creating a new track every time.
    let (dups, integrates) = Either.partitionEithers $ map is_dup $
            Seq.group_stable Derive.integrated_source (Cmd.perf_integrated perf)
        is_dup (x :| xs) = if null xs then Right x else Left x
    unless (null dups) $
        Log.warn $ "these blocks or tracks want to integrate twice: "
            <> Text.intercalate ", "
                (map (either pretty pretty . Derive.integrated_source) dups)
    mapM_ (integrate block_id) integrates
    return Cmd.Continue
cmd_integrate _ = return Cmd.Continue

-- | Integrate the track information into the current state.
integrate :: Cmd.M m => BlockId -> Derive.Integrated -> m ()
integrate derived_block_id integrated = do
    tracks <- Convert.convert derived_block_id
        (Derive.integrated_events integrated)
    case Derive.integrated_source integrated of
        Left block_id -> integrate_block block_id tracks
        Right track_id -> integrate_tracks derived_block_id track_id tracks

-- | Update and replace the DeriveDestinations for the given TrackId.
-- A source track can have multiple destinations, and each of those is actually
-- a list of DeriveDestinations.
integrate_tracks :: Cmd.M m => BlockId -> TrackId -> Convert.Tracks -> m ()
integrate_tracks block_id track_id tracks = do
    itracks <- Block.block_integrated_tracks <$> Ui.get_block block_id
    let dests =
            [ dests
            | (source_id, Block.DeriveDestinations dests) <- itracks
            , source_id == track_id
            ]
    new_dests <- if null dests
        then (:[]) <$> Merge.merge_tracks Merge.KeepTitles block_id tracks []
        else mapM (Merge.merge_tracks Merge.KeepTitles block_id tracks) dests
    unless (null new_dests) $
        Log.notice $ "derive integrated " <> showt track_id <> " to "
            <> pretty (map (map (fst . Block.dest_note)) new_dests)
    Ui.modify_integrated_tracks block_id $ replace track_id
        [(track_id, Block.DeriveDestinations dests) | dests <- new_dests]
    Cmd.derive_immediately [block_id]

-- | Look for blocks derived from this one and replace their contents, or
-- create a new block if there are no blocks derived from this one.
integrate_block :: Cmd.M m => BlockId -> Convert.Tracks -> m ()
integrate_block source_id tracks = do
    blocks <- Ui.gets Ui.state_blocks
    dest_blocks <- case integrated_from blocks of
        [] -> do
            (block_id, dests) <- Merge.create_block source_id tracks
            Create.view block_id
            return [(block_id, dests)]
        integrated -> forM integrated $ \(dest_id, track_dests) ->
            (,) dest_id <$> Merge.merge_block dest_id tracks track_dests
    Log.notice $ "derive integrated " <> showt source_id <> " to "
        <> pretty (map fst dest_blocks)
    forM_ dest_blocks $ \(dest_block_id, track_dests) ->
        Ui.set_integrated_block dest_block_id $
            Just (source_id, Block.DeriveDestinations track_dests)
    Cmd.derive_immediately (map fst dest_blocks)
    where
    integrated_from blocks =
        [ (block_id, dests)
        | (block_id, Just (source_block, Block.DeriveDestinations dests))
            <- map (second Block.block_integrated) (Map.toList blocks)
        , source_block == source_id
        ]


-- * score integrate

-- | For each block with 'Block.ScoreDestinations', figure out if their sources
-- have damage, and if so, re-integrate.
score_integrate :: [Update.UiUpdate] -> Ui.State
    -> Either Ui.Error ([Log.Msg], Ui.State, Update.UiDamage)
score_integrate updates state = Ui.run_id state $ do
    -- These both use the passed state instead of using Ui.get when figuring
    -- out if there are updates that require integration.  This way, a
    -- track integrate can't trigger a block integrate, at least not until the
    -- next call to this function.
    track_logs <- concatMapM score_integrate_tracks $
        needs_track_score_integrate updates state
    block_logs <- mapM score_integrate_block $
        needs_block_score_integrate updates state
    return $ map (Log.msg Log.Notice Nothing) (track_logs ++ block_logs)

score_integrate_block :: Ui.M m => BlockId -> m Text
score_integrate_block source_id = do
    blocks <- Ui.gets Ui.state_blocks
    let integrated = integrated_from blocks
    forM_ integrated $ \(dest_id, dests) -> do
        dests <- Merge.score_merge_block source_id dest_id dests
        Ui.set_integrated_block dest_id $
            Just (source_id, Block.ScoreDestinations dests)
    return $ "score integrated " <> showt source_id <> " to: "
        <> pretty (map fst integrated)
    where
    integrated_from blocks =
        [ (block_id, dests)
        | (block_id, Just (source_block, Block.ScoreDestinations dests))
            <- map (second Block.block_integrated) (Map.toList blocks)
        , source_block == source_id
        ]

score_integrate_tracks :: Ui.M m => (BlockId, TrackId) -> m [Text]
score_integrate_tracks (block_id, track_id) = do
    itracks <- Block.block_integrated_tracks <$> Ui.get_block block_id
    let dests =
            [ dests
            | (source_id, Block.ScoreDestinations dests) <- itracks
            , source_id == track_id
            ]
    new_dests <- mapM (Merge.score_merge_tracks block_id track_id) dests
    Ui.modify_integrated_tracks block_id $ replace track_id
        [(track_id, Block.ScoreDestinations dests) | dests <- new_dests]
    return $ map msg new_dests
    where
    msg dests = "score integrated " <> showt track_id <> ": "
        <> Text.intercalate ", "
            [ pretty source_id <> " -> " <> pretty dest_id
            | (source_id, (dest_id, _)) <- dests
            ]

replace :: Eq key => key -> [(key, a)] -> [(key, a)] -> [(key, a)]
replace key new xs = new ++ filter ((/=key) . fst) xs

-- | Blocks which are block score integrate sources and have damage.
needs_block_score_integrate :: [Update.UiUpdate] -> Ui.State -> [BlockId]
needs_block_score_integrate updates state =
    filter has_integrated $ Map.keys $ flip Map.restrictKeys damaged_blocks $
        Ui.state_blocks state
    where
    -- TODO this is a linear search through all blocks
    has_integrated block_id = not $ null
        [ ()
        | Just (dest_block_id, Block.ScoreDestinations {}) <-
            map Block.block_integrated $ Map.elems (Ui.state_blocks state)
        , block_id == dest_block_id
        ]
    damaged_blocks = Set.fromList $ mapMaybe block_changed updates
    block_changed (Update.Block bid _) = Just bid
    block_changed _ = Nothing

-- | Tracks which are track score integrate sources and have damage.
needs_track_score_integrate :: [Update.UiUpdate] -> Ui.State
    -> [(BlockId, TrackId)]
needs_track_score_integrate updates state = Seq.unique $
    concatMap (integrated_blocks . fst) $ mapMaybe Update.track_changed updates
    where
    integrated_blocks track_id =
        [ (block_id, track_id) | (block_id, block) <- blocks_with track_id
        , has_integrated block track_id
        ]
    -- TODO this is a linear search through all blocks, as is
    -- Ui.blocks_with_track_id.
    blocks_with track_id = filter (has_track track_id . snd) $ Map.toList $
        Ui.state_blocks state
    has_track track_id block = track_id `elem` Block.block_track_ids block
    has_integrated block track_id = not $ null
        [ ()
        | (source_track_id, Block.ScoreDestinations {})
            <- Block.block_integrated_tracks block
        , track_id == source_track_id
        ]

-- * manual integrate

-- | Find blocks with the source key, and merge the given tracks into them.
--
-- If you are creating a new track, you need to have already done that and put
-- an empty destination in it.
manual_integrate :: Ui.M m => Block.SourceKey -> Convert.Track -- ^ note track
    -> [Convert.Track] -- ^ dependent control tracks
    -> m ()
manual_integrate key note controls = do
    block_dests <- manual_destinations key . Map.toList <$>
        Ui.gets Ui.state_blocks
    forM_ block_dests $ \(block_id, dests) -> do
        new_dests <- forM dests $ \dest ->
            Merge.merge_tracks Merge.KeepTitles block_id [(note, controls)]
                [dest]
        Ui.set_integrated_manual block_id key (Just (concat new_dests))

-- | Find all manual derive destinations with the given key.
manual_destinations :: Block.SourceKey -> [(a, Block.Block)]
    -> [(a, [Block.NoteDestination])]
manual_destinations key = filter (not . null . snd)
    . map (second (Map.findWithDefault [] key . Block.block_integrated_manual))
