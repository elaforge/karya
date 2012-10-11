module Cmd.RulerUtil where
import qualified Data.Map as Map

import Util.Control
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State

import qualified Cmd.Meter as Meter
import qualified App.Config as Config
import Types


-- * constructors

-- | Constructor for "plain" rulers.
ruler :: [(Ruler.Name, Ruler.Marklist)] -> Ruler.Ruler
ruler marklists = Ruler.Ruler
    { Ruler.ruler_marklists = Map.fromList marklists
    , Ruler.ruler_bg = Config.ruler_bg
    , Ruler.ruler_show_names = False
    , Ruler.ruler_align_to_bottom = False
    }

-- | Create a ruler with a meter of the given duration.
meter_ruler :: ScoreTime -> Meter.AbstractMeter -> Ruler.Ruler
meter_ruler dur meter =
    ruler [(Meter.meter, Meter.meter_marklist (Meter.fit_meter dur meter))]

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
local_meter block_id f = modify_block block_id (Meter.modify_meter f)

-- | Modify a meter on all blocks that have it.
modify_meter :: (State.M m) => RulerId -> (Meter.Meter -> Meter.Meter) -> m ()
modify_meter ruler_id f = State.modify_ruler ruler_id (Meter.modify_meter f)

get_meter :: (State.M m) => RulerId -> m Meter.Meter
get_meter = fmap Meter.ruler_meter . State.get_ruler

-- * local modify

modify_block :: (State.M m) => BlockId -> (Ruler.Ruler -> Ruler.Ruler) -> m ()
modify_block block_id f = mapM_ (\ruler_id -> local_modify block_id ruler_id f)
    =<< State.rulers_of block_id

-- TODO two techniques: modify a copy, or modify the original.  I should have
-- a modify for both

-- | Modify the given RulerId, making a new one if it's already in use on
-- a block other than the give one.
local_modify :: (State.M m) => BlockId -> RulerId -> (Ruler.Ruler -> Ruler.Ruler)
    -> m RulerId
local_modify block_id ruler_id f = do
    blocks <- State.tracks_with_ruler_id ruler_id
    case blocks of
        [(rblock_id, _)] | block_id == rblock_id -> do
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
