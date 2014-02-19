-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Cmd-level support for integration.  These cmds interpret the output of
    the calls in "Derive.Call.Integrate" to create score from deriver output
    and merge it back into the current score.

    An example of track integration:

    - Add \" | <\" to a note track title, which causes damage and a rederive.

    - Integrate call collects events and puts them into derive results, which
    go into DeriveComplete, which winds up at 'integrate_tracks'.

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

    - Track damage causes a rederive, which causes the `\<` call to collect
    integrated events.

    - 'integrate_tracks' merges the changes into the destination track (or
    tracks), which damages them.

    - This time when the derive happens, since there was no damage on the
    source track, it gets cached.  The cache intentionally doesn't retain
    integrated events, so `\<` is skipped and I don't get a second derivation.

    Block integration is similar, except that I don't get a double derivation
    when the first new block is created, since the damage is separated to
    a different block.

    It might be a little more orthogonal to omit the thing where
    I automatically create an integrated block or track if there are none, but
    it's convenient in practice.  It does, however, make it tricky to undo
    past the integrate, since if you undo the block\/track creation, the
    integrate call is still there and just creates another.  Be quick!
-}
module Cmd.Integrate (cmd_integrate, integrate, score_integrate) where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import Types


cmd_integrate :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmd_integrate (Msg.DeriveStatus block_id (Msg.DeriveComplete perf))
    | null (Cmd.perf_integrated perf) = return Cmd.Continue
    | otherwise = do
        integrated <- concatMapM (integrate block_id) (Cmd.perf_integrated perf)
        -- TODO filter out DeriveDestinations
        State.modify_integrated_tracks block_id (const integrated)
        return Cmd.Continue
cmd_integrate _ = return Cmd.Continue

-- | Integrate the track information into the current state, and if it
-- was a track integrate, return the TrackDestinations.  If it was a block
-- integrate, the TrackDestination has already been put into the
-- 'Block.block_integrated'.  The difference is because there can be multiple
-- track integrations, and I want to replace them all at once.
integrate :: Cmd.M m => BlockId -> Derive.Integrated
    -> m [(TrackId, Block.TrackDestinations)]
integrate block_id integrated = do
    tracks <- Convert.convert block_id (Derive.integrated_events integrated)
    case Derive.integrated_source integrated of
        Left block_id -> do
            integrate_block block_id tracks
            return []
        Right track_id -> map (second Block.DeriveDestinations) <$>
            integrate_tracks block_id track_id tracks

integrate_tracks :: Cmd.M m => BlockId -> TrackId -> Convert.Tracks
    -> m [(TrackId, [Block.DeriveDestination])]
integrate_tracks block_id track_id tracks = do
    itracks <- Block.block_integrated_tracks <$> State.get_block block_id
    let dests = [dests | (tid, Block.DeriveDestinations dests) <- itracks,
            tid == track_id]
    new_dests <- if null dests
        then (:[]) <$> Merge.merge_tracks block_id tracks []
        else mapM (Merge.merge_tracks block_id tracks) dests
    Log.notice $ "integrated " <> show track_id <> " to: " <> pretty new_dests
    Cmd.derive_immediately [block_id]
    return $ map ((,) track_id) new_dests

-- | Look for blocks derived from this one and replace their contents, or
-- create a new block if there are no blocks derived from this one.
integrate_block :: Cmd.M m => BlockId -> Convert.Tracks -> m ()
integrate_block source_id tracks = do
    blocks <- State.gets State.state_blocks
    new_blocks <- case integrated_from blocks of
        [] -> do
            (block_id, dests) <- Merge.create_block source_id tracks
            Create.view block_id
            return [(block_id, dests)]
        integrated -> forM integrated $ \(dest_id, track_dests) ->
            (,) dest_id <$> Merge.merge_block dest_id tracks track_dests
    Log.notice $ "integrated " <> show source_id <> " to: "
        <> pretty (map fst new_blocks)
    forM_ new_blocks $ \(new_block_id, track_dests) ->
        unless (null track_dests) $
            State.set_integrated_block new_block_id $
                Just (source_id, Block.DeriveDestinations track_dests)
    Cmd.derive_immediately (map fst new_blocks)
    where
    integrated_from blocks =
        [ (block_id, dests)
        | (block_id, Just (source_block, Block.DeriveDestinations dests)) <-
            map (second Block.block_integrated) (Map.toList blocks)
        , source_block == source_id
        ]


-- * score integrate

score_integrate :: [Update.UiUpdate] -> State.State
    -> Either State.Error ([Log.Msg], State.State, [Update.CmdUpdate])
score_integrate updates state = State.run_id state $ do
    -- These both use the passed state instead of using State.get when figuring
    -- out if there are updates that require integration.  This way, a
    -- track integrate can't trigger a block integrate, at least not until the
    -- next call to this function.
    track_logs <- mapM score_track_integrate $
        needs_track_integrate updates state
    block_logs <- mapM score_integrate_block $
        needs_block_integrate updates state
    return $ map (Log.msg Log.Debug Nothing) (track_logs ++ block_logs)

score_integrate_block :: State.M m => BlockId -> m Text
score_integrate_block source_id = do
    blocks <- State.gets State.state_blocks
    new_blocks <- case integrated_from blocks of
        [] -> do
            block_id <- Merge.score_create_block source_id
            -- TODO Create.view requires screen dimensions from Cmd.State.
            Create.unfitted_view block_id
            return [block_id]
        integrated -> do
            forM_ integrated $ \(dest_id, dests) ->
                Merge.score_merge_block source_id dest_id dests
            return $ map fst integrated
    return $ "score integrated " <> showt source_id <> " to: "
        <> prettyt new_blocks
    where
    integrated_from blocks =
        [ (block_id, dests)
        | (block_id, Just (source_block, Block.ScoreDestinations dests)) <-
            map (second Block.block_integrated) (Map.toList blocks)
        , source_block == source_id
        ]

score_track_integrate :: State.M m => (BlockId, TrackId) -> m Text
score_track_integrate (block_id, track_id) = do
    itracks <- Block.block_integrated_tracks <$> State.get_block block_id
    let dests = [dests | (tid, Block.ScoreDestinations dests) <- itracks,
            tid == track_id]
    new_dests <- if null dests
        then (:[]) <$> Merge.score_merge_tracks block_id track_id []
        else mapM (Merge.score_merge_tracks block_id track_id) dests
    -- TODO replace only ScoreDestinations with tid==track_id
    State.modify_integrated_tracks block_id $
        const [(track_id, Block.ScoreDestinations dests) | dests <- new_dests]
    return $ "score integrated " <> showt track_id <> " to: "
        <> prettyt new_dests

needs_block_integrate :: [Update.UiUpdate] -> State.State -> [BlockId]
needs_block_integrate updates state =
    map fst $ filter damaged $ Map.toList (State.state_blocks state)
    where
    damaged (block_id, block) = block_id `elem` block_ids
        && Merge.block_has_score_inegrate (Block.block_title block)
    block_ids = mapMaybe block_changed updates

needs_track_integrate :: [Update.UiUpdate] -> State.State
    -> [(BlockId, TrackId)]
needs_track_integrate updates state =
    concat $ mapMaybe (damaged <=< Update.track_changed) updates
    where
    damaged (track_id, _) = do
        track <- Map.lookup track_id (State.state_tracks state)
        guard (Merge.track_has_score_integrate (Track.track_title track))
        return [(block_id, track_id) | block_id <- blocks_with track_id]
    blocks_with track_id = map fst $ filter (has track_id . snd) $
        Map.toList $ State.state_blocks state
    has track_id block = track_id `elem` Block.block_track_ids block

block_changed :: Update.UiUpdate -> Maybe BlockId
block_changed (Update.Block bid _) = Just bid
block_changed _ = Nothing
