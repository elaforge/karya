{-# LANGUAGE NoMonomorphismRestriction #-}
{- | Work with rulers and meters.  A meter is just a ruler specialized to
    display regular numbered subdivisions.

    Most of the functions in here operate on a local copy of the ruler, i.e.
    they will modify the ruler on the given block, making a copy if it is
    shared with other blocks.  This is not as composable as it could be, e.g.
    you could pass the transformations to a modify or modify_local function,
    but for the moment modifying the local ruler and then copying it over to
    other blocks as needed is convenient.

    Examples:

    - Give the current block the standard 4/4 meter.  Since m44 is 4 measures
    of 4/4, stretching by 16 gives each whole note 1t.

        > LRuler.modify =<< LRuler.fit_to_pos 16 Meter.m44

    - Or put the selection at the where the 4 meters should end and run

        > LRuler.modify =<< LRuler.fit_to_selection Meter.m44

    - Make the last measure 5/4 by selecting a quarter note and running
      @LRuler.append@.

    - TODO make a middle measure 5/4?

    - TODO: inspect a meter
-}
module Cmd.Lang.LRuler where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Meter as Meter
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.RulerUtil as RulerUtil
import qualified Cmd.Selection as Selection

import Types


-- * general purpose

rename :: RulerId -> RulerId -> Cmd.CmdL ()
rename = Create.rename_ruler

-- | List all rulers, along with the number of blocks each one appears in.
list :: Cmd.CmdL [(RulerId, Int)]
list = do
    ruler_ids <- State.all_ruler_ids
    counts <- map length <$> mapM State.blocks_with_ruler_id ruler_ids
    return $ zip ruler_ids counts

-- | Destroy all unrefereced rulers, and return their now-invalid RulerIds.
gc :: (State.M m) => m [RulerId]
gc = do
    ruler_ids <- Create.orphan_rulers
    mapM_ State.destroy_ruler ruler_ids
    return ruler_ids

-- | Group together rulers that are the same, replace all the duplicates with
-- the first ruler in each group, then gc away the duplicates.  Return the
-- duplicates.
unify :: Cmd.CmdL [[RulerId]]
unify = do
    groups <- Seq.group_eq snd <$>
        State.gets (Map.toAscList . State.state_rulers)
    mapM_ unify groups
    gc
    return $ map (map fst) groups
    where
    unify ((rid, _) : dups) = forM_ (map fst dups) $ \dup_rid ->
        replace_ruler_id dup_rid rid
    unify _ = return ()

-- | Blocks that contain the given ruler.
blocks_of :: (State.M m) => RulerId -> m [BlockId]
blocks_of = fmap (map fst) . State.blocks_with_ruler_id

-- | Set the rulers on a block to the given RulerId.
set_ruler_id :: (State.M m) => RulerId -> BlockId -> m ()
set_ruler_id ruler_id block_id = do
    old <- State.block_ruler block_id
    State.replace_ruler_id block_id old ruler_id

-- | Replace all occurrences of one RulerId with another.
replace_ruler_id :: (State.M m) => RulerId -> RulerId -> m ()
replace_ruler_id old new = do
    blocks <- State.blocks_with_ruler_id old
    forM_ (map fst blocks) $ \block_id ->
        State.replace_ruler_id block_id old new

-- | Fix up old states with overlay rulers.
fix :: Cmd.CmdL [RulerId]
fix = do
    rids <- State.all_ruler_ids
    forM_ rids $ \r -> State.modify_ruler r $ \ru ->
        ru { Ruler.ruler_show_names = False }
    block_ids <- State.all_block_ids
    forM_ block_ids $ \block_id -> do
        r <- State.ruler_of block_id
        Create.set_block_ruler r block_id
    gc

-- | Double the meter of the current block. You can then trim it down to size.
double :: (Cmd.M m) => m Modify
double = do
    block_id <- Cmd.get_focused_block
    return (block_id, \meter -> meter <> meter)

-- | Clip the meter to the selection.
clip :: (Cmd.M m) => m Modify
clip = do
    (block_id, _, _, pos) <- Selection.get_insert
    return (block_id, Meter.clip 0 pos)

-- | Copy the meter under the selection and append it to the end of the ruler.
append :: (Cmd.M m) => m Modify
append = do
    (block_id, _, _, start, end) <- Selection.tracks
    return (block_id, \meter -> meter <> Meter.clip start end meter)

append_ruler_id :: (Cmd.M m) => RulerId -> m Modify
append_ruler_id ruler_id = do
    block_id <- Cmd.get_focused_block
    other <- Meter.ruler_meter <$> State.get_ruler ruler_id
    return (block_id, (<> other))

-- | Remove the selected range of the ruler and shift the rest up.
delete :: (Cmd.M m) => m Modify
delete = do
    (block_id, _, _, start, end) <- Selection.tracks
    return (block_id, Meter.remove start end)

-- | Set the ruler to a number of measures of the given meter, where each
-- measure is the given amount of time.
measures :: (Cmd.M m) => TrackTime -- ^ duration of one measure
    -> Meter.AbstractMeter -> Int -- ^ measures per section
    -> Int -- ^ sections
    -> m Modify
measures dur meter measures sections =
    fit_to_pos (dur * fromIntegral (measures * sections)) $
        replicate sections (Meter.repeat measures meter)

-- | Replace the meter on this block, fitted to the end of the last event on
-- the block.
fit_to_end :: (Cmd.M m) => [Meter.AbstractMeter] -> BlockId -> m Modify
fit_to_end meter block_id = do
    end <- State.block_event_end block_id
    fit_to_pos end meter

fit_to_selection :: (Cmd.M m) => [Meter.AbstractMeter] -> m Modify
fit_to_selection meter = do
    (_, _, _, pos) <- Selection.get_insert
    fit_to_pos pos meter

fit_to_pos :: (Cmd.M m) => ScoreTime -> [Meter.AbstractMeter] -> m Modify
fit_to_pos pos meters = do
    when (pos <= 0) $
        Cmd.throw "can't set ruler for block with 0 duration"
    block_id <- Cmd.get_focused_block
    return (block_id, const $ Meter.fit_meter pos meters)

get_meter :: (State.M m) => BlockId -> m Meter.Meter
get_meter block_id =
    Meter.ruler_meter <$> (State.get_ruler =<< State.ruler_of block_id)

-- * extract

extract :: (Cmd.M m) => m Modify
extract = do
    (block_id, _, track_id, _) <- Selection.get_insert
    all_meters <- extract_meters track_id
    return (block_id, const all_meters)

-- | Extract the meter marklists from the sub-blocks called on the given
-- track, concatenate them, and replace the current meter with it.
extract_meters :: (Cmd.M m) => TrackId -> m Meter.Meter
extract_meters track_id = do
    subs <- extract_calls track_id
    ruler_ids <- mapM State.ruler_of [bid | (_, _, bid) <- subs]
    meters <- mapM RulerUtil.get_meter ruler_ids
    return $ concat [Meter.scale dur meter
        | ((_start, dur, _), meter) <- zip subs meters]

extract_calls :: (State.M m) => TrackId -> m [(ScoreTime, ScoreTime, BlockId)]
extract_calls track_id = do
    events <- Events.ascending . Track.track_events <$>
        State.get_track track_id
    ns <- State.get_namespace
    let call = NoteTrack.block_call ns . Event.event_string
    return $ do
        event <- events
        Just block_id <- return (call event)
        return (Event.start event, Event.duration event, block_id)

-- * modify

type Modify = (BlockId, Meter.Meter -> Meter.Meter)

-- | Just like 'RulerUtil.local_meter' but invalidate performances.  Since
-- block calls use the ruler length to determine the duration of the block,
-- changing the ruler can affect the performance.
--
-- I don't add this directly to 'RulerUtil.local_meter' because that would make
-- it be in Cmd and IO.
local :: Modify -> Cmd.CmdL ()
local (block_id, f) = RulerUtil.local_meter block_id f

modify :: Modify -> Cmd.CmdL ()
modify (block_id, f) = RulerUtil.modify_meter block_id f


-- * cue

cue :: Ruler.Name
cue = "cue"

-- | Drop a mark at the selected point in the "cue" ruler.
add_cue :: String -> Cmd.CmdL ()
add_cue text = do
    (block_id, _, _, pos) <- Selection.get_insert
    add_cue_at block_id pos text

add_cue_at :: BlockId -> ScoreTime -> String -> Cmd.CmdL ()
add_cue_at block_id pos text = modify_block block_id $
    Ruler.modify_marklist cue $ Ruler.insert_mark pos (cue_mark text)

cue_mark :: String -> Ruler.Mark
cue_mark text = Ruler.Mark 0 2 Color.black text 0 0

modify_block :: BlockId -> (Ruler.Ruler -> Ruler.Ruler) -> Cmd.CmdL ()
modify_block = RulerUtil.modify_block
