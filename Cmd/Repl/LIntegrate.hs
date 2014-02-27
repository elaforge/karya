-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
module Cmd.Repl.LIntegrate where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.Selection as Selection

import Types


-- | Create an integrated block from the focused block.  The block integrate
-- call will automatically create one block, but you have to use this if you
-- want more than one.  Actually, you can use it on a block without a `<<`
-- integrate call, but there isn't much point since it won't reintegrate until
-- you add one.
block :: Cmd.M m => m ViewId
block = do
    source_block <- Cmd.get_focused_block
    ruler_id <- State.block_ruler source_block
    dest_block <- Create.block ruler_id
    State.set_integrated_block dest_block $
        Just (source_block, Block.DeriveDestinations [])
    Cmd.derive_immediately [source_block]
    Cmd.inflict_block_damage source_block
    Create.view dest_block

score_block :: Cmd.M m => m ViewId
score_block = do
    source_block <- Cmd.get_focused_block
    ruler_id <- State.block_ruler source_block
    dest_block <- Create.block ruler_id
    State.set_integrated_block dest_block $
        Just (source_block, Block.ScoreDestinations [])
    Cmd.inflict_block_damage source_block
    Create.view dest_block

-- | Similar to 'block', explicitly create another track integrated from the
-- selected one, which should already have a `<` integrate call on it.
track :: Cmd.M m => m ()
track = do
    (block_id, _, track_id, _) <- Selection.get_insert
    State.modify_integrated_tracks block_id $
        ((track_id, Block.DeriveDestinations []) :)
    Cmd.derive_immediately [block_id]
    Cmd.inflict_track_damage block_id track_id

score_track :: Cmd.M m => m ()
score_track = do
    (block_id, _, track_id, _) <- Selection.get_insert
    State.modify_integrated_tracks block_id $
        ((track_id, Block.ScoreDestinations []) :)
    Cmd.inflict_track_damage block_id track_id

sel_edits :: Cmd.CmdL ([Event.IndexKey], [Merge.Edit])
sel_edits = do
    (block_id, _, track_id, _) <- Selection.get_insert
    edits block_id track_id

edits :: Cmd.M m => BlockId -> TrackId -> m ([Event.IndexKey], [Merge.Edit])
edits block_id track_id = do
    block <- State.get_block block_id
    index <- Cmd.require "track is not integrated from anywhere" $
        lookup track_id $ indices_of (Block.block_integrated block)
            (Block.block_integrated_tracks block)
    events <- State.get_all_events track_id
    let (deleted, edits) = Merge.diff_events index events
    return (Set.toList deleted, filter Merge.is_modified edits)

indices_of :: Maybe (BlockId, Block.TrackDestinations)
    -> [(TrackId, Block.TrackDestinations)] -> [(TrackId, Block.EventIndex)]
indices_of integrated integrated_tracks =
    block_indices ++ concatMap dest_indices integrated_tracks
    where
    block_indices = maybe [] dest_indices integrated
    dest_indices (_, Block.DeriveDestinations dests) =
        concatMap derive_indices dests
    dest_indices (_, Block.ScoreDestinations dests) = map snd dests
    derive_indices (Block.DeriveDestination note controls) =
        note : Map.elems controls
