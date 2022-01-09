-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ruler.Extract (pull_up, push_down, push_down_recursive) where
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Seq as Seq
import qualified Cmd.NoteTrackParse as NoteTrackParse
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Derive.ParseTitle as ParseTitle
import qualified Ui.Event as Event
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ui as Ui

import           Global
import           Types


-- * pull_up

-- | Extract the meter marklists from the sub-blocks called on the given
-- track and concatenate them.
pull_up :: Ui.M m => BlockId -> TrackId -> m Meter.Meter
pull_up block_id track_id = do
    subs <- block_calls block_id track_id
    meters <- mapM (RulerUtil.get_meter <=< Ui.ruler_of . snd) subs
    -- TODO previously I would scale the meters by the size of the event,
    -- but if I assume calls are 1:1 then this isn't necessary
    return $ mconcat meters

block_calls :: Ui.M m => BlockId -> TrackId -> m [(Event.Event, BlockId)]
block_calls block_id track_id = map (second NonEmpty.head) <$>
    NoteTrackParse.track_block_calls False block_id track_id


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
     -> BlockId -> TrackId -> m [BlockId] -- ^ modified children
push_down not_1to1_ok block_id track_id = do
    (subs, not_1to1) <- sub_meters block_id track_id
    unless (not_1to1_ok || null not_1to1) $
        Ui.throw $ "block calls not 1:1: " <> pretty not_1to1
    let sub_blocks = Seq.drop_dups fst subs
    forM_ sub_blocks $ \(block_id, meter) ->
        RulerUtil.local_meter RulerUtil.Block block_id (const meter)
    return $ map fst sub_blocks

push_down_recursive :: Ui.M m => Bool -> BlockId -> TrackId -> m ()
push_down_recursive not_1to1_ok block_id track_id = do
    children <- push_down not_1to1_ok block_id track_id
    forM_ children $ \child_block -> do
        track_ids <- filterM is_note =<< Ui.track_ids_of child_block
        mapM_ (push_down_recursive not_1to1_ok child_block) track_ids
    where
    is_note = fmap ParseTitle.is_note_track . Ui.get_track_title

sub_meters :: Ui.M m => BlockId -> TrackId
    -> m ([(BlockId, Meter.Meter)], [BlockId])
sub_meters block_id track_id = do
    subs <- block_calls block_id track_id
    (subs, not_1to1) <- partitionM
        (\(event, callee) -> is_1to1 (Event.duration event) callee)
        subs
    subs <- forM subs $ \(event, sub_block) -> do
        meter <- extract_meter block_id (Event.start event) (Event.end event)
        return (sub_block, meter)
    return (subs, map snd not_1to1)

extract_meter :: Ui.M m => BlockId -> TrackTime -> TrackTime -> m Meter.Meter
extract_meter block_id start end = do
    meter <- RulerUtil.get_meter =<< Ui.ruler_of block_id
    -- TODO previously I would set config_start_measure to attempt to make the
    -- sub-meter start at an appropriate count, but now I won't bother unless I
    -- need it.
    return $ Meter.modify_sections (RulerUtil.extract start end) meter

is_1to1 :: Ui.M m => TrackTime -> BlockId -> m Bool
is_1to1 dur block_id = (==dur) <$> Ui.block_end block_id
