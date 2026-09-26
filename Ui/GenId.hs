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


viewId :: Ui.M m => BlockId -> m Id.Id
viewId blockId =
    requireId "view id" . generateViewId blockId =<< Ui.gets Ui.stateViews

blockId :: Ui.M m => Maybe BlockId -> m Id.Id
blockId maybeParent = do
    ns <- Ui.getNamespace
    requireId "block id" . generateBlockId maybeParent ns
        =<< Ui.gets Ui.stateBlocks

trackId :: Ui.M m => BlockId -> m Id.Id
trackId blockId = requireId "track id" . generateTrackId blockId
    =<< Ui.gets Ui.stateTracks

-- | ViewIds look like \"ns/b0.v0\", \"ns/b0.v1\", etc.
generateViewId :: BlockId -> Map ViewId a -> Maybe Id.Id
generateViewId bid views =
    generateId (Id.idNamespace ident) ident "v" Id.ViewId views
    where ident = Id.unpackId bid

generateBlockId :: Maybe BlockId -> Id.Namespace -> Map BlockId a
    -> Maybe Id.Id
generateBlockId maybeParent ns blocks =
    generateId ns parent "b" Id.BlockId blocks
    where parent = maybe (Id.global "") Id.unpackId maybeParent

generateTrackId :: BlockId -> Map TrackId a -> Maybe Id.Id
generateTrackId bid tracks =
    generateId (Id.idNamespace ident) ident "t" Id.TrackId tracks
    where ident = Id.unpackId bid

generateId :: Ord a => Id.Namespace -> Id.Id -> Text -> (Id.Id -> a)
    -> Map a b -> Maybe Id.Id
generateId ns parentId code typ fm =
    List.find (not . (`Map.member` fm) . typ) candidates
    where candidates = idsFor ns (Id.idName parentId) code

-- | Guess a TrackId generated from GenId.  Useful for tests.
trackIdAt :: BlockId -> TrackNum -> TrackId
trackIdAt bid tracknum = Id.TrackId $ idAt ns ident "t" tracknum
    where (ns, ident) = Id.unId $ Id.unpackId bid

-- | IDs are numbered, and they start at 1 instead of 0.
--
-- This is because usually tracknum 0 is the ruler, so counting with tracknums,
-- event tracks start at 1.  The actual TrackId should be irrelevant (and would
-- be out of date as soon as a track is swapped), but for testing it's very
-- convenient if they line up with the tracknums.  So even though it's purely
-- for testing and only for TrackIds, I start everything at 1 just for
-- consistency.
idsFor :: Id.Namespace -> Text -> Text -> [Id.Id]
idsFor ns parent code = map (idAt ns parent code) [1..]

idAt :: Id.Namespace -> Text -> Text -> Int -> Id.Id
idAt ns parent code n =
    Id.id ns (dotted parent <> code <> showt n)
    where dotted s = if Text.null s then "" else s <> "."

requireId :: Ui.M m => Text -> Maybe a -> m a
requireId msg = maybe (Ui.throw $ "somehow can't find ID for " <> msg) pure
