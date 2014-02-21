-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
module Cmd.Repl.LRuler where
import qualified Prelude
import Prelude hiding (concat)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
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
listn :: Cmd.CmdL [(RulerId, Int)]
listn = map (second length) <$> list

list :: State.M m => m [(RulerId, [BlockId])]
list = do
    ruler_ids <- State.all_ruler_ids
    block_ids <- mapM State.blocks_with_ruler_id ruler_ids
    return $ zip ruler_ids (map (map fst) block_ids)

-- | Destroy all unrefereced rulers, and return their now-invalid RulerIds.
gc :: State.M m => m [RulerId]
gc = do
    ruler_ids <- Create.orphan_rulers
    mapM_ State.destroy_ruler ruler_ids
    return ruler_ids

-- | Group together rulers that are the same, replace all the duplicates with
-- the first ruler in each group, then gc away the duplicates.  Return the
-- duplicates.
unify :: State.M m => m [[RulerId]]
unify = do
    groups <- Seq.group_eq_on snd <$>
        State.gets (Map.toAscList . State.state_rulers)
    mapM_ merge groups
    gc
    return $ filter ((>1) . length) $ map (map fst . NonEmpty.toList) groups
    where
    merge ((rid, _) :| dups) = forM_ (map fst dups) $ \dup_rid ->
        replace_ruler_id dup_rid rid

-- | After copying blocks around and fiddling with rulers, the RulerIds can
-- wind up with names from other blocks.  Synchronize RulerIds along with their
-- owning BlockIds.  A RulerId only on one BlockId is assumed to be local to
-- that block, and will get its name.
sync_ids :: State.M m => m Text
sync_ids = do
    deleted <- unify
    let unified = if null deleted then "" else Text.unlines $
            "Unified:" : [prettyt x <> " <- " <> prettyt xs | x : xs <- deleted]
            ++ [""]
    misnamed <- list_misnamed
    let renames = [(ruler_id, RulerUtil.block_id_to_ruler block_id)
            | (ruler_id, block_id) <- misnamed]
    Create.rename_rulers renames
    let renamed = if null renames then "" else Text.unlines $
            "Renamed:" : [ prettyt from <> " -> "
                <> prettyt (Id.RulerId to) | (from, to) <- renames]
    return $ unified <> renamed

list_misnamed :: State.M m => m [(RulerId, BlockId)]
list_misnamed = go <$> list
    where
    go ruler_blocks =
        [ (ruler_id, block_id)
        | (ruler_id, Just block_id) <- map (second len1) ruler_blocks
        , RulerUtil.block_id_to_ruler block_id /= Id.unpack_id ruler_id
        ]
    len1 [x] = Just x
    len1 _ = Nothing

-- | Blocks that contain the given ruler.
blocks_of :: (State.M m) => RulerId -> m [BlockId]
blocks_of = fmap (map fst) . State.blocks_with_ruler_id

-- | Set the rulers on a block to the given RulerId.
set_ruler_id :: (State.M m) => RulerId -> BlockId -> m ()
set_ruler_id ruler_id block_id = do
    old <- State.block_ruler block_id
    State.replace_ruler_id block_id old ruler_id

local_ruler :: (Cmd.M m) => Ruler.Ruler -> m ()
local_ruler ruler = do
    block_id <- Cmd.get_focused_block
    RulerUtil.local_block block_id (const ruler)

-- | Replace all occurrences of one RulerId with another.
replace_ruler_id :: (State.M m) => RulerId -> RulerId -> m ()
replace_ruler_id old new = do
    blocks <- State.blocks_with_ruler_id old
    forM_ (map fst blocks) $ \block_id ->
        State.replace_ruler_id block_id old new

-- * query

get_meter :: (State.M m) => BlockId -> m Meter.Meter
get_meter block_id =
    Meter.ruler_meter <$> (State.get_ruler =<< State.ruler_of block_id)

get_marks :: (State.M m) => BlockId -> m [Ruler.PosMark]
get_marks block_id =
    Ruler.ascending 0 . Ruler.get_marklist Ruler.meter <$>
        (State.get_ruler =<< State.ruler_of block_id)

-- * Modify

-- | Double the meter of the current block. You can then trim it down to size.
double :: (Cmd.M m) => m Modify
double = do
    block_id <- Cmd.get_focused_block
    -- The final 0 duration mark should be replaced by the first mark.
    return (block_id, \meter -> Seq.rdrop 1 meter <> meter)

-- | Clip the meter to end at the selection.
clip :: (Cmd.M m) => m DeleteModify
clip = do
    (block_id, _, _, pos) <- Selection.get_insert
    return (block_id, Meter.clip 0 (Meter.time_to_duration pos))

-- | Copy the meter under the selection and append it to the end of the ruler.
append :: (Cmd.M m) => m Modify
append = do
    (block_id, _, _, start, end) <- Selection.tracks
    return (block_id, \meter ->
        meter <> Meter.clipm (Meter.time_to_duration start)
            (Meter.time_to_duration end) meter)

append_ruler_id :: (Cmd.M m) => RulerId -> m Modify
append_ruler_id ruler_id = do
    block_id <- Cmd.get_focused_block
    other <- Meter.ruler_meter <$> State.get_ruler ruler_id
    return (block_id, (<> other))

-- | Remove the selected range of the ruler and shift the rest up.
delete :: (Cmd.M m) => m DeleteModify
delete = do
    (block_id, _, _, start, end) <- Selection.tracks
    return (block_id, Meter.delete
        (Meter.time_to_duration start) (Meter.time_to_duration end))

-- | Strip out ranks below a certain value, for the whole block.  Larger scale
-- blocks don't need the fine resolution and can wind up with huge rulers.
strip_ranks :: Cmd.M m => Meter.RankName -> m DeleteModify
strip_ranks max_rank = do
    block_id <- Cmd.get_focused_block
    return (block_id, Meter.strip_ranks (Meter.name_to_rank max_rank))

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
    return (block_id,
        const $ Meter.fit_meter (Meter.time_to_duration pos) meters)

-- | Replace the meter with the concatenation of the rulers of the given
-- blocks.  This is like 'extract' except it doesn't infer the blocks from the
-- calls and doesn't scale the extracted rulers.
concat :: Cmd.M m => [BlockId] -> m Modify
concat block_ids = do
    ruler_ids <- mapM State.ruler_of block_ids
    -- Strip the last 0-dur mark off of each meter before concatenating.
    meters <- map (Seq.rdrop 1) <$> mapM RulerUtil.get_meter ruler_ids
    let meter = mconcat meters ++ [(0, 0)]
    block_id <- Cmd.get_focused_block
    return (block_id, const meter)

-- * extract

extract :: (Cmd.M m) => m Modify
extract = do
    (block_id, _, track_id, _) <- Selection.get_insert
    all_meters <- extract_meters block_id track_id
    return (block_id, const all_meters)

-- | Extract the meter marklists from the sub-blocks called on the given
-- track, concatenate them, and replace the current meter with it.
extract_meters :: (Cmd.M m) => BlockId -> TrackId -> m Meter.Meter
extract_meters block_id track_id = do
    subs <- extract_calls block_id track_id
    ruler_ids <- mapM State.ruler_of [bid | (_, _, bid) <- subs]
    -- Strip the last 0-dur mark off of each meter before concatenating.
    meters <- map (Seq.rdrop 1) <$> mapM RulerUtil.get_meter ruler_ids
    return $ mconcat $
        [ Meter.scale (Meter.time_to_duration dur) meter
        | ((_start, dur, _), meter) <- zip subs meters
        ] ++ [[(0, 0)]]

extract_calls :: (State.M m) => BlockId -> TrackId
    -> m [(ScoreTime, ScoreTime, BlockId)]
extract_calls block_id track_id =
    mapMaybeM extract =<< Events.ascending . Track.track_events <$>
        State.get_track track_id
    where
    extract event = fmap (range event) <$>
        NoteTrack.block_call (Just block_id) (Event.event_text event)
    range event block_id = (Event.start event, Event.duration event, block_id)

-- * modify

-- | A modification of a simple numbered meter.  It regenerates the mark
-- numbers, so if you had anything more fancy in there it will be destroyed.
-- 'Meter.Meterlike' has details.
type Modify = ModifyM Meter.Meter

-- | A modification that only deletes marks and doesn't generate new ones,
-- so it can work on a LabeledMeter.  See 'Meter.Meterlike' for details.
type DeleteModify = ModifyM Meter.LabeledMeter

type ModifyM m = (BlockId, m -> m)

-- | Just like 'RulerUtil.local_meter' but invalidate performances.  Since
-- block calls use the ruler length to determine the duration of the block,
-- changing the ruler can affect the performance.
--
-- I don't add this directly to 'RulerUtil.local_meter' because that would make
-- it be in Cmd and IO.
local :: (Meter.Meterlike m) => ModifyM m -> Cmd.CmdL ()
local (block_id, f) = RulerUtil.local_meter block_id f

modify :: (Meter.Meterlike m) => ModifyM m -> Cmd.CmdL ()
modify (block_id, f) = RulerUtil.modify_meter block_id f


-- * cue

cue :: Ruler.Name
cue = "cue"

-- | Drop a mark at the selected point in the \"cue\" ruler.
add_cue :: Text -> Cmd.CmdL ()
add_cue text = do
    (block_id, _, _, pos) <- Selection.get_insert
    add_cue_at block_id pos text

remove_cues :: Cmd.CmdL ()
remove_cues = do
    block_id <- Cmd.get_focused_block
    RulerUtil.local_block block_id $ Ruler.remove_marklist cue

add_cue_at :: BlockId -> ScoreTime -> Text -> Cmd.CmdL ()
add_cue_at block_id pos text = RulerUtil.local_block block_id $
    Ruler.modify_marklist cue $ Ruler.insert_mark pos (cue_mark text)

cue_mark :: Text -> Ruler.Mark
cue_mark text = Ruler.Mark 0 2 Color.black text 0 0
