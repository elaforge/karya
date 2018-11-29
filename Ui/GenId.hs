-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to create a non-colliding Id, using the standard naming scheme.
-- It should be passed to the apppropriate create funciton, e.g.
-- 'Ui.create_block' to make a BlockId.
module Ui.GenId where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Ui.Id as Id
import qualified Ui.Ui as Ui

import Global
import Types


view_id :: Ui.M m => BlockId -> m Id.Id
view_id block_id =
    require_id "view id" . generate_view_id block_id =<< Ui.gets Ui.state_views

block_id :: Ui.M m => Maybe BlockId -> m Id.Id
block_id maybe_parent = do
    ns <- Ui.get_namespace
    require_id "block id" . generate_block_id maybe_parent ns
        =<< Ui.gets Ui.state_blocks

track_id :: Ui.M m => BlockId -> m Id.Id
track_id block_id = require_id "track id" . generate_track_id block_id "t"
    =<< Ui.gets Ui.state_tracks

-- | ViewIds look like \"ns/b0.v0\", \"ns/b0.v1\", etc.
generate_view_id :: BlockId -> Map ViewId _a -> Maybe Id.Id
generate_view_id bid views =
    generate_id (Id.id_namespace ident) ident "v" Id.ViewId views
    where ident = Id.unpack_id bid

generate_block_id :: Maybe BlockId -> Id.Namespace -> Map BlockId _a
    -> Maybe Id.Id
generate_block_id maybe_parent ns blocks =
    generate_id ns parent "b" Id.BlockId blocks
    where parent = maybe (Id.global "") Id.unpack_id maybe_parent

generate_track_id :: BlockId -> Text -> Map TrackId _a -> Maybe Id.Id
generate_track_id bid code tracks =
    generate_id (Id.id_namespace ident) ident code Id.TrackId tracks
    where ident = Id.unpack_id bid

generate_id :: Ord a => Id.Namespace -> Id.Id -> Text -> (Id.Id -> a)
    -> Map a _b -> Maybe Id.Id
generate_id ns parent_id code typ fm =
    List.find (not . (`Map.member` fm) . typ) candidates
    where candidates = ids_for ns (Id.id_name parent_id) code

-- | IDs are numbered, and they start at 1 instead of 0.
--
-- This is because usually tracknum 0 is the ruler, so counting with tracknums,
-- event tracks start at 1.  The actual TrackId should be irrelevant (and would
-- be out of date as soon as a track is swapped), but for testing it's very
-- convenient if they line up with the tracknums.  So even though it's purely
-- for testing and only for TrackIds, I start everything at 1 just for
-- consistency.
ids_for :: Id.Namespace -> Text -> Text -> [Id.Id]
ids_for ns parent code =
    [Id.id ns (dotted parent <> code <> showt n) | n <- [1..]]
    where dotted s = if Text.null s then "" else s <> "."

require_id :: Ui.M m => Text -> Maybe a -> m a
require_id msg = maybe (Ui.throw $ "somehow can't find ID for " <> msg) return
