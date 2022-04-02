-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to deal with derive and score integration.
module Cmd.Repl.LIntegrate where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ui as Ui

import           Global
import           Types


-- * create

-- | Create an integrated block from the focused block.  The block integrate
-- call will automatically create one block, but you have to use this if you
-- want more than one.  Actually, you can use it on a block without a `<<`
-- integrate call, but there isn't much point since it won't reintegrate until
-- you add one.
block :: Cmd.M m => m ViewId
block = do
    source_block <- Cmd.get_focused_block
    ruler_id <- Ui.block_ruler source_block
    dest_block <- Create.block ruler_id
    Ui.set_integrated_block dest_block $
        Just (source_block, Block.DeriveDestinations [])
    Cmd.derive_immediately [source_block]
    Cmd.inflict_block_damage source_block
    Create.view dest_block

-- | Create a block integrate copy of the selected block.  Details at
-- 'Block.ScoreDestinations'.
score_block :: Cmd.M m => m ViewId
score_block = do
    source_block <- Cmd.get_focused_block
    ruler_id <- Ui.block_ruler source_block
    dest_block <- Create.block ruler_id
    Ui.set_integrated_block dest_block $
        Just (source_block, Block.ScoreDestinations [])
    Cmd.inflict_block_damage source_block
    Create.view dest_block

-- | Similar to 'block', explicitly create another track integrated from the
-- selected one, which should already have a `<` integrate call on it.
track :: Cmd.M m => m ()
track = do
    (block_id, _, track_id, _) <- Selection.get_insert
    Ui.modify_integrated_tracks block_id
        ((track_id, Block.DeriveDestinations []) :)
    Cmd.derive_immediately [block_id]
    Cmd.inflict_track_damage block_id track_id

-- | Create a track integrate copy of the selected track.  Details at
-- 'Block.ScoreDestinations'.
score_track :: Cmd.M m => m ()
score_track = do
    (block_id, _, track_id, _) <- Selection.get_insert
    Ui.modify_integrated_tracks block_id
        ((track_id, Block.ScoreDestinations []) :)
    Cmd.inflict_track_damage block_id track_id


-- * revert

-- | Revert the selected range back to the integrated state.
sel_revert :: Cmd.M m => m ()
sel_revert = do
    (block_id, _, track_ids, range) <- Selection.tracks
    Edit.clear_range track_ids range
    by_dest <- Block.destination_to_source <$> Ui.get_block block_id
    sequence_
        [ Ui.insert_block_events block_id track_id
            (map Event.unmodified (Map.elems index))
        | (track_id, (_, index)) <- by_dest
        , track_id `elem` track_ids
        ]

delete_manual :: Cmd.M m => Block.SourceKey -> m ()
delete_manual key = do
    block_id <- Cmd.get_focused_block
    Ui.set_integrated_manual block_id key Nothing


-- * inspect

-- | Show the integration state in an abbreviated way.
sources :: Cmd.M m => m [(TrackId, (Block.Source, Text))]
sources = do
    block <- Ui.get_block =<< Cmd.get_focused_block
    return $ map (fmap (fmap Block.short_event_index)) $
        Block.destination_to_source block

sel_edits :: Cmd.M m => m ([Event.IndexKey], [Merge.Edit])
sel_edits = do
    (block_id, _, track_id, _) <- Selection.get_insert
    edits block_id track_id

edits :: Cmd.M m => BlockId -> TrackId -> m ([Event.IndexKey], [Merge.Edit])
edits block_id track_id = do
    block <- Ui.get_block block_id
    index <- Cmd.require "track is not integrated from anywhere" $
        lookup track_id $ indices_of (Block.block_integrated block)
            (Block.block_integrated_tracks block)
    events <- Ui.get_events track_id
    let (deleted, edits) = Merge.diff_events index (Events.ascending events)
    return (Set.toList deleted, filter Merge.is_modified edits)

-- | Show source UI events.
indices :: Cmd.M m => m [(TrackId, (Block.Source, Block.EventIndex))]
indices =
    fmap Block.destination_to_source . Ui.get_block =<< Cmd.get_focused_block

indices_of :: Maybe (BlockId, Block.TrackDestinations)
    -> [(TrackId, Block.TrackDestinations)] -> [(TrackId, Block.EventIndex)]
indices_of integrated integrated_tracks =
    block_indices ++ concatMap dest_indices integrated_tracks
    where
    block_indices = maybe [] dest_indices integrated
    dest_indices (_, Block.DeriveDestinations dests) =
        concatMap derive_indices dests
    dest_indices (_, Block.ScoreDestinations dests) = map snd dests
    derive_indices (Block.NoteDestination _ note controls) =
        note : Map.elems controls

integrated :: Cmd.M m => m Text
integrated = do
    integrated <- Cmd.perf_integrated <$> Perf.get_root
    return $ Text.unlines $ concatMap fmt integrated
    where
    fmt (Derive.Integrated source events) =
        pretty source : Stream.short_events events
