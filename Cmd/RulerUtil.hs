-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Infrastructure for dealing with rulers in a higher level way than as
-- a 'Ruler.Marklist'.
module Cmd.RulerUtil where
import qualified Data.List as List
import qualified Data.Map as Map

import Util.Control
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State

import qualified Cmd.Meter as Meter
import Types


-- * constructors

-- | Create a ruler with a meter of the given duration.
meter_ruler :: Int -> Meter.Duration -> [Meter.AbstractMeter] -> Ruler.Ruler
meter_ruler start_at dur meters = Ruler.meter_ruler $
    Meter.meter_marklist start_at (Meter.fit_meter dur meters)

-- | Replace or add a marklist with the given name.
set_marklist :: State.M m => RulerId -> Ruler.Name -> Ruler.Marklist -> m ()
set_marklist ruler_id name mlist =
    State.modify_ruler ruler_id (Ruler.set_marklist name mlist)

-- | Copy a marklist from one ruler to another.  If it already exists in
-- the destination ruler, it will be replaced.
copy_marklist :: State.M m => Ruler.Name -> RulerId -> RulerId -> m ()
copy_marklist name from_ruler_id to_ruler_id = do
    from_ruler <- State.get_ruler from_ruler_id
    set_marklist to_ruler_id name $ Ruler.get_marklist name from_ruler

-- * meter

-- | Modify the meter locally to a block, i.e. the ruler will be copied if it
-- is shared with other blocks.
local_meter :: (State.M m, Meter.Meterlike meter) =>
    BlockId -> (meter -> meter) -> m ()
local_meter block_id f = local_block block_id (Meter.modify_meter f)

-- | Modify a meter destructively.
modify_meter :: (State.M m, Meter.Meterlike meter) =>
    BlockId -> (meter -> meter) -> m ()
modify_meter block_id = modify_block block_id . Meter.modify_meter

get_meter :: State.M m => RulerId -> m Meter.Meter
get_meter = fmap Meter.ruler_meter . State.get_ruler

-- * local modify

local_block :: State.M m => BlockId -> (Ruler.Ruler -> Ruler.Ruler) -> m ()
local_block block_id modify =
    mapM_ (\ruler_id -> local block_id ruler_id modify)
        =<< State.rulers_of block_id

modify_block :: State.M m => BlockId -> (Ruler.Ruler -> Ruler.Ruler) -> m ()
modify_block block_id modify = do
    ruler_id <- State.ruler_of block_id
    if ruler_id == State.no_ruler
        then local_block block_id modify
        else State.modify_ruler ruler_id modify

-- | Modify the given RulerId, making a new one if it's already in use on
-- a block other than the give one.
local :: State.M m => BlockId -> RulerId
    -> (Ruler.Ruler -> Ruler.Ruler) -> m RulerId
local block_id ruler_id f = do
    blocks <- State.blocks_with_ruler_id ruler_id
    case blocks of
        [(rblock_id, _)]
            | ruler_id /= State.no_ruler && block_id == rblock_id -> do
                State.modify_ruler ruler_id f
                return ruler_id
        _ -> do
            -- Try to the ruler the same name as the block, since it's now
            -- local to that block, appending numbers if the name is taken.
            -- This can happen if this block is a copy of another with a local
            -- RulerId.
            new_ruler_id <- block_id_to_free_ruler <$>
                State.gets State.state_rulers <*> return block_id
            new_ruler_id <- copy new_ruler_id ruler_id
            State.modify_ruler new_ruler_id f
            sequence_ $ do
                (rblock_id, tracks) <- blocks
                guard (rblock_id == block_id)
                (tracknum, _) <- tracks
                return $ State.set_track_ruler block_id tracknum new_ruler_id
            return new_ruler_id

-- | Create a block-local RulerId.  It gets the exact same name as the block,
-- but is defined as a function so it's clear where this conversion is done.
block_id_to_ruler :: BlockId -> Id.Id
block_id_to_ruler = Id.unpack_id

block_id_to_free_ruler :: Map.Map RulerId a -> BlockId -> Id.Id
block_id_to_free_ruler rulers block_id = id
    where
    -- Always just since the list is infinite.
    Just id = List.find (not . (`Map.member` rulers) . Id.RulerId) $
        map (Id.id ns) $ name : [name <> "-" <> showt n | n <- [1..]]
    (ns, name) = Id.un_id $ Id.unpack_id block_id

copy :: (State.M m) => Id.Id -> RulerId -> m RulerId
copy ident ruler_id = State.create_ruler ident =<< State.get_ruler ruler_id
