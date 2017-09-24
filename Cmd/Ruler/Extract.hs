-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ruler.Extract (pull_up, push_down) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Ruler as Ruler
import qualified Ui.Ui as Ui

import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Modify as Modify
import qualified Cmd.Ruler.RulerUtil as RulerUtil

import Global
import Types


-- * pull_up

-- | Extract the meter marklists from the sub-blocks called on the given
-- track and concatenate them.
pull_up :: Ui.M m => BlockId -> TrackId -> m Meter.LabeledMeter
pull_up block_id track_id = do
    subs <- block_calls block_id track_id
    ruler_ids <- mapM (Ui.ruler_of . snd) subs
    -- Strip the last 0-dur mark off of each meter before concatenating.
    meters <- map (Seq.rdrop 1) <$> mapM RulerUtil.get_meter ruler_ids
    return $ mconcat $
        [ Meter.scale (Meter.time_to_duration (Event.duration event)) meter
        | ((event, _), meter) <- zip subs meters
        ] ++ [[RulerUtil.final_mark]]

block_calls :: Ui.M m => BlockId -> TrackId -> m [(Event.Event, BlockId)]
block_calls block_id track_id = map (second NonEmpty.head) <$>
    NoteTrack.track_block_calls False block_id track_id


-- * push_down

-- | The inverse of 'pull_up': find callee blocks, and copy the ruler from the
-- given block to them.  This sets the 'Ruler.config_start_measure'
-- appropriately, so subsequent modifications should keep the measure numbers.
--
-- If a block is called more than once, it will get the measure number from the
-- first occurrence.
--
-- Since this has to modify multiple blocks, it does the modification itself
-- instead of returning the new meter like 'pull_up'.
push_down :: Ui.M m => Bool
    -- ^ Whether or not it's an error if there are block calls which are not
    -- 1:1.  I can't tell if that's an error or not, but the user should know
    -- if it's supposed to be a \"score\" block.
    --
    -- TODO Optionally I could scale the ruler for non-1:1 callees.
     -> BlockId -> TrackId -> m ()
push_down not_1to1_ok block_id track_id = do
    (subs, not_1to1) <- sub_meters block_id track_id
    unless (not_1to1_ok || null not_1to1) $
        Ui.throw $ "block calls not 1:1: " <> pretty not_1to1
    mapM_ set (Seq.drop_dups fst subs)
    where
    set (block_id, (config, marks)) = RulerUtil.local RulerUtil.Block block_id $
        Right . Ruler.set_meter config marks

sub_meters :: Ui.M m => BlockId -> TrackId
    -> m ([(BlockId, (Ruler.MeterConfig, Ruler.Marklist))], [BlockId])
sub_meters block_id track_id = do
    subs <- block_calls block_id track_id
    (subs, not_1to1) <- partitionM
        (\(event, callee) -> is_1to1 (Event.duration event) callee)
        subs
    subs <- forM subs $ \(event, sub_block) -> do
        (config, meter) <- extract_meter block_id
            (Event.start event) (Event.end event)
        return (sub_block, (config, Meter.labeled_marklist meter))
    return (subs, map snd not_1to1)

extract_meter :: Ui.M m => BlockId -> TrackTime -> TrackTime
    -> m (Ruler.MeterConfig, Meter.LabeledMeter)
extract_meter block_id start end = do
    (config, meter) <- get_meter block_id
    Ui.require "" $ extract_marks config start end $
        Meter.marklist_labeled meter

extract_marks :: Ruler.MeterConfig -> TrackTime -> TrackTime
    -> Meter.LabeledMeter -> Maybe (Ruler.MeterConfig, Meter.LabeledMeter)
extract_marks m_config start end meter = do
    config <- Map.lookup (Ruler.config_name m_config) Modify.configs
    let (start_measure, extracted) =
            Meter.extract_with_measure config start end meter
    return (m_config { Ruler.config_start_measure = start_measure }, extracted)

get_meter :: Ui.M m => BlockId -> m (Ruler.MeterConfig, Ruler.Marklist)
get_meter = fmap Ruler.get_meter . Ui.get_ruler <=< Ui.ruler_of

is_1to1 :: Ui.M m => TrackTime -> BlockId -> m Bool
is_1to1 dur block_id = (==dur) <$> Ui.block_end block_id

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = go
    where
    go (x:xs) = ifM (f x) (first (x:) <$> go xs) (return ([], x:xs))
    go [] = return ([], [])
