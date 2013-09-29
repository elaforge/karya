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
module Cmd.Integrate (cmd_integrate, integrate) where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import Types


cmd_integrate :: (Cmd.M m) => Msg.Msg -> m Cmd.Status
cmd_integrate (Msg.DeriveStatus block_id (Msg.DeriveComplete perf))
    | null (Cmd.perf_integrated perf) = return Cmd.Continue
    | otherwise = do
        integrated <- concatMapM (integrate block_id) (Cmd.perf_integrated perf)
        State.modify_integrated_tracks block_id (const integrated)
        return Cmd.Continue
cmd_integrate _ = return Cmd.Continue

-- | Integrate the track information into the current state, and if it
-- was a track integrate, return the TrackDestinations.  If it was a block
-- integrate, the TrackDestination has already been put into the
-- 'Block.block_integrated'.  TODO why the discrepancy?
integrate :: (Cmd.M m) => BlockId -> Derive.Integrated
    -> m [(TrackId, [Block.TrackDestination])]
integrate block_id integrated = do
    tracks <- Convert.convert block_id (Derive.integrated_events integrated)
    case Derive.integrated_source integrated of
        Left block_id -> do
            integrate_block block_id tracks
            return []
        Right track_id -> integrate_tracks block_id track_id tracks

integrate_tracks :: (Cmd.M m) => BlockId -> TrackId -> Convert.Tracks
    -> m [(TrackId, [Block.TrackDestination])]
integrate_tracks block_id track_id tracks = do
    itracks <- Block.block_integrated_tracks <$> State.get_block block_id
    new_dests <- case filter ((==track_id) . fst) itracks of
        [] -> (:[]) <$> Merge.merge_tracks block_id tracks []
        dests -> mapM (Merge.merge_tracks block_id tracks . snd) dests
    Log.notice $ "integrated " <> show track_id <> " to: "
        <> Pretty.pretty new_dests
    Cmd.derive_immediately [block_id]
    return $ map ((,) track_id) new_dests

-- | Look for blocks derived from this one and replace their contents, or
-- create a new block if there are no blocks derived from this one.
integrate_block :: (Cmd.M m) => BlockId -> Convert.Tracks -> m ()
integrate_block block_id tracks = do
    blocks <- State.gets State.state_blocks
    new_blocks <- case integrated_from block_id blocks of
        [] -> do
            (block_id, dests) <- Merge.create_block block_id tracks
            Create.view block_id
            return [(block_id, dests)]
        integrated -> forM integrated $ \(block_id, track_dests) ->
            (,) block_id <$> Merge.merge_block block_id tracks track_dests
    Log.notice $ "integrated " <> show block_id <> " to: "
        <> Pretty.pretty (map fst new_blocks)
    forM_ new_blocks $ \(new_block_id, track_dests) ->
        unless (null track_dests) $
            State.set_integrated_block new_block_id $
                Just (block_id, track_dests)
    Cmd.derive_immediately (map fst new_blocks)
    where
    integrated_from source_block_id blocks =
        [ (block_id, dests)
        | (block_id, Just (source_block, dests)) <-
            map (second Block.block_integrated) (Map.toList blocks)
        , source_block == source_block_id
        ]
