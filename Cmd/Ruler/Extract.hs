-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ruler.Extract (extract, inject) where
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.Ui as Ui

import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.RulerUtil as RulerUtil

import Global
import Types


-- * extract

-- | Extract the meter marklists from the sub-blocks called on the given
-- track and concatenate them.
extract :: Ui.M m => BlockId -> TrackId -> m Meter.LabeledMeter
extract block_id track_id = do
    subs <- block_calls block_id track_id
    ruler_ids <- mapM Ui.ruler_of [bid | (_, _, bid) <- subs]
    -- Strip the last 0-dur mark off of each meter before concatenating.
    meters <- map (Seq.rdrop 1) <$> mapM RulerUtil.get_meter ruler_ids
    return $ mconcat $
        [ Meter.scale (Meter.time_to_duration dur) meter
        | ((_start, dur, _), meter) <- zip subs meters
        ] ++ [[RulerUtil.final_mark]]

block_calls :: Ui.M m => BlockId -> TrackId
    -> m [(TrackTime, TrackTime, BlockId)]
block_calls block_id track_id =
    mapMaybeM extract =<< Events.ascending <$> Ui.get_events track_id
    where
    extract event = fmap (range event) <$>
        NoteTrack.block_call (Just block_id) (Event.text event)
    range event block_id = (Event.start event, Event.duration event, block_id)


-- * inject

-- | The inverse of 'extact': find callee blocks, and copy the ruler from the
-- given block to them.  This sets the ruler start count appropriately.
--
-- If a callee occurs only once, and is 1:1 with the caller, copy the caller's
-- ruler.  Otherwise warn about that call and skip it.
--
-- Optionally I could scale the ruler for non-1:1 callees.
--
-- Since this has to modify multiple blocks, it does the modification itself
-- instead of returning the new meter like 'extract'.
inject :: Ui.M m => Bool
    -- ^ Whether or not it's an error if there are block calls which are not
    -- 1:1.  I can't tell if that's an error or not, but the user should know
    -- if it's supposed to be a \"score\" block.
     -> BlockId -> TrackId -> m ()
inject not_1to1_ok block_id track_id = do
    (subs, not_1to1) <- sub_meters block_id track_id
    unless (not_1to1_ok || null not_1to1) $
        Ui.throw $ "block calls not 1:1: " <> pretty not_1to1
    mapM_ set subs
    where
    set (block_id, (mtype, marks)) = RulerUtil.local RulerUtil.Block block_id $
        Right . Ruler.set_marklist Ruler.meter mtype marks

sub_meters :: Ui.M m => BlockId -> TrackId
    -> m ([(BlockId, (Maybe Ruler.MeterType, Ruler.Marklist))], [BlockId])
sub_meters block_id track_id = do
    subs <- block_calls block_id track_id
    (subs, not_1to1) <- partitionM (\(_, dur, callee) -> is_1to1 dur callee)
        subs
    (mtype, meter) <- get_meter block_id
    let get (start, dur, block_id) =
            (block_id, (mtype, extract_marks start dur meter))
    return (map get subs, [block_id | (_, _, block_id) <- not_1to1])

extract_marks :: TrackTime -> TrackTime -> Ruler.Marklist -> Ruler.Marklist
extract_marks start dur =
    Ruler.marklist . takeWhile ((<=dur) . fst) . map (first (subtract start))
    . Ruler.ascending start

get_meter :: Ui.M m => BlockId -> m (Maybe Ruler.MeterType, Ruler.Marklist)
get_meter = fmap (Ruler.get_marklist Ruler.meter) . Ui.get_ruler <=< Ui.ruler_of

is_1to1 :: Ui.M m => TrackTime -> BlockId -> m Bool
is_1to1 dur block_id = (==dur) <$> Ui.block_end block_id

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = go
    where
    go (x:xs) = ifM (f x) (first (x:) <$> go xs) (return ([], x:xs))
    go [] = return ([], [])
