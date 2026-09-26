-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ruler.Extract (pullUp, pushDown, pushDownRecursive) where
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Lists as Lists
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
pullUp :: Ui.M m => BlockId -> TrackId -> m Meter.Meter
pullUp blockId trackId = do
    subs <- blockCalls blockId trackId
    meters <- mapM (RulerUtil.getMeter <=< Ui.rulerOf . snd) subs
    -- TODO previously I would scale the meters by the size of the event,
    -- but if I assume calls are 1:1 then this isn't necessary
    pure $ mconcat meters

blockCalls :: Ui.M m => BlockId -> TrackId -> m [(Event.Event, BlockId)]
blockCalls blockId trackId = map (second NonEmpty.head) <$>
    NoteTrackParse.trackBlockCalls False blockId trackId


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
pushDown :: Ui.M m => Bool
    -- ^ Whether or not it's an error if there are block calls which are not
    -- 1:1.  I can't tell if that's an error or not, but the user should know
    -- if it's supposed to be a \"score\" block.
    --
    -- TODO Optionally I could scale the ruler for non-1:1 callees.
     -> BlockId -> TrackId -> m [BlockId] -- ^ modified children
pushDown not1to1Ok blockId trackId = do
    (subs, not1to1) <- subMeters blockId trackId
    unless (not1to1Ok || null not1to1) $
        Ui.throw $ "block calls not 1:1: " <> pretty not1to1
    let subBlocks = Lists.dropDups fst subs
    forM_ subBlocks $ \(blockId, meter) ->
        RulerUtil.localMeter RulerUtil.Block blockId (const meter)
    pure $ map fst subBlocks

pushDownRecursive :: Ui.M m => Bool -> BlockId -> TrackId -> m ()
pushDownRecursive not1to1Ok blockId trackId = do
    children <- pushDown not1to1Ok blockId trackId
    forM_ children $ \childBlock -> do
        trackIds <- filterM isNote =<< Ui.trackIdsOf childBlock
        mapM_ (pushDownRecursive not1to1Ok childBlock) trackIds
    where
    isNote = fmap ParseTitle.isNoteTrack . Ui.getTrackTitle

subMeters :: Ui.M m => BlockId -> TrackId
    -> m ([(BlockId, Meter.Meter)], [BlockId])
subMeters blockId trackId = do
    subs <- blockCalls blockId trackId
    (subs, not1to1) <- partitionM
        (\(event, callee) -> is1to1 (Event.duration event) callee)
        subs
    subs <- forM subs $ \(event, subBlock) -> do
        meter <- extractMeter blockId (Event.start event) (Event.end event)
        pure (subBlock, meter)
    pure (subs, map snd not1to1)

extractMeter :: Ui.M m => BlockId -> TrackTime -> TrackTime -> m Meter.Meter
extractMeter blockId start end = do
    meter <- RulerUtil.getMeter =<< Ui.rulerOf blockId
    -- TODO previously I would set config_start_measure to attempt to make the
    -- sub-meter start at an appropriate count, but now I won't bother unless I
    -- need it.
    pure $ Meter.modifySections (RulerUtil.extract start end) meter

is1to1 :: Ui.M m => TrackTime -> BlockId -> m Bool
is1to1 dur blockId = (==dur) <$> Ui.blockEnd blockId
