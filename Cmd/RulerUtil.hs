-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.RulerUtil where
import Util.Control
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State

import qualified Cmd.Meter as Meter
import Types


-- * constructors

-- | Create a ruler with a meter of the given duration.
meter_ruler :: ScoreTime -> [Meter.AbstractMeter] -> Ruler.Ruler
meter_ruler dur meters = Ruler.ruler
    [(Meter.meter, Meter.meter_marklist (Meter.fit_meter dur meters))]

-- | Replace or add a marklist with the given name.
set_marklist :: (State.M m) => RulerId -> Ruler.Name -> Ruler.Marklist -> m ()
set_marklist ruler_id name mlist =
    State.modify_ruler ruler_id (Ruler.set_marklist name mlist)

-- | Copy a marklist from one ruler to another.  If it already exists in
-- the destination ruler, it will be replaced.
copy_marklist :: (State.M m) => Ruler.Name -> RulerId -> RulerId -> m ()
copy_marklist name from_ruler_id to_ruler_id = do
    from_ruler <- State.get_ruler from_ruler_id
    set_marklist to_ruler_id name $ Ruler.get_marklist name from_ruler

-- * meter

-- | Modify the meter locally to a block, i.e. the ruler will be copied if it
-- is shared with other blocks.
local_meter :: (State.M m) => BlockId -> (Meter.Meter -> Meter.Meter) -> m ()
local_meter block_id f = local_block block_id (Meter.modify_meter f)

-- | Modify a meter destructively.
modify_meter :: (State.M m) => BlockId -> (Meter.Meter -> Meter.Meter) -> m ()
modify_meter block_id f = do
    ruler_id <- State.ruler_of block_id
    State.modify_ruler ruler_id (Meter.modify_meter f)

get_meter :: (State.M m) => RulerId -> m Meter.Meter
get_meter = fmap Meter.ruler_meter . State.get_ruler

-- * local modify

local_block :: (State.M m) => BlockId -> (Ruler.Ruler -> Ruler.Ruler) -> m ()
local_block block_id f = mapM_ (\ruler_id -> local block_id ruler_id f)
    =<< State.rulers_of block_id

-- | Modify the given RulerId, making a new one if it's already in use on
-- a block other than the give one.
local :: (State.M m) => BlockId -> RulerId
    -> (Ruler.Ruler -> Ruler.Ruler) -> m RulerId
local block_id ruler_id f = do
    blocks <- State.blocks_with_ruler_id ruler_id
    case blocks of
        [(rblock_id, _)]
            | ruler_id /= State.no_ruler && block_id == rblock_id -> do
                State.modify_ruler ruler_id f
                return ruler_id
        _ -> do
            -- Give the ruler the same name as the block, since it's now local
            -- to that block.
            new_ruler_id <- copy (Id.unpack_id block_id) ruler_id
            State.modify_ruler new_ruler_id f
            sequence_ $ do
                (rblock_id, tracks) <- blocks
                guard (rblock_id == block_id)
                (tracknum, _) <- tracks
                return $ State.set_track_ruler block_id tracknum new_ruler_id
            return new_ruler_id

copy :: (State.M m) => Id.Id -> RulerId -> m RulerId
copy ident ruler_id = State.create_ruler ident =<< State.get_ruler ruler_id
