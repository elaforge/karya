{- | Work with rulers and meters.  A meter is just a ruler specialized to
    display regular numbered subdivisions.

    Most of the functions in here operate on a local copy of the ruler, i.e.
    they will modify the ruler on the given block, making a copy if it is
    shared with other blocks.  This is not as composable as it could be, e.g.
    you could pass the transformations to a modify or modify_local function,
    but for the moment modifying the local ruler and then copying it over to
    other blocks as needed is convenient.

    Examples:

    - Give the current block the standard 4/4 meter.  Since m44 is 4 measures of
      4/4, stretching by 16 gives each whole note 1t.

      > LRuler.set_meter 16 Meter.m44 =<< block

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

-- | Destroy all unrefereced rulers, and return their now-invalid RulerIds.
gc :: (State.M m) => m [RulerId]
gc = do
    ruler_ids <- Create.orphan_rulers
    mapM_ State.destroy_ruler ruler_ids
    return ruler_ids

-- | Blocks that contain the given ruler.
blocks_of :: (State.M m) => RulerId -> m [BlockId]
blocks_of = fmap (map fst) . State.tracks_with_ruler_id

set_ruler_id :: (State.M m) => RulerId -> BlockId -> m ()
set_ruler_id ruler_id block_id = do
    old <- State.block_ruler block_id
    State.replace_ruler_id block_id old ruler_id

-- | Group together rulers that are the same, replace all the duplicates with
-- the first ruler in each group, then gc away the duplicates.
unify_rulers :: Cmd.CmdL [[RulerId]]
unify_rulers = do
    groups <- Seq.group_eq snd <$>
        State.gets (Map.toAscList . State.state_rulers)
    mapM_ unify groups
    gc
    return $ map (map fst) groups
    where
    unify ((rid, _) : dups) = forM_ (map fst dups) $ \dup_rid ->
        replace_ruler_id dup_rid rid
    unify _ = return ()

-- | Replace all occurrences of one RulerId with another.
replace_ruler_id :: (State.M m) => RulerId -> RulerId -> m ()
replace_ruler_id old new = do
    blocks <- State.tracks_with_ruler_id old
    forM_ (map fst blocks) $ \block_id ->
        State.replace_ruler_id block_id old new

-- | Double the meter of the current block. You can then trim it down to size.
double :: Cmd.CmdL ()
double = do
    block_id <- Cmd.get_focused_block
    local_meter block_id $ \meter -> meter <> meter

-- | Clip the meter to the selection.
clip :: Cmd.CmdL ()
clip = do
    (block_id, _, _, pos) <- Selection.get_insert
    local_meter block_id $ Meter.clip 0 pos

-- | Copy the meter under the selection and append it to the end of the ruler.
append :: Cmd.CmdL ()
append = do
    (block_id, _, _, start, end) <- Selection.tracks
    local_meter block_id $ \meter -> meter <> Meter.clip start end meter

append_ruler_id :: RulerId -> Cmd.CmdL ()
append_ruler_id ruler_id = do
    block_id <- Cmd.get_focused_block
    other <- Meter.ruler_meter <$> State.get_ruler ruler_id
    local_meter block_id (<> other)

-- | Remove the selected ruler.
delete :: Cmd.CmdL ()
delete = do
    (block_id, _, _, start, end) <- Selection.tracks
    ruler_end <- State.block_ruler_end block_id
    local_meter block_id $ \meter ->
        Meter.clip 0 start meter <> Meter.clip end ruler_end meter

-- | Replace the meter for the rulers of this block, fitted to the end of the
-- last event on the block.
fitted_meter :: Meter.AbstractMeter -> BlockId -> Cmd.CmdL ()
fitted_meter meter block_id = do
    dur <- State.block_event_end block_id
    when (dur <= 0) $
        Cmd.throw $ "can't set ruler for block with 0 duration"
    local_meter block_id $ const $ Meter.fit_meter dur meter

set_meter :: ScoreTime -> Meter.AbstractMeter -> BlockId -> Cmd.CmdL ()
set_meter dur meter block_id =
    local_meter block_id $ const $ Meter.fit_meter dur meter

get_meter :: (State.M m) => BlockId -> m Meter.Meter
get_meter block_id =
    Meter.ruler_meter <$> (State.get_ruler =<< State.ruler_of block_id)

-- | Just like 'RulerUtil.local_meter' but invalidate performances.  Since
-- block calls use the ruler length to determine the duration of the block,
-- changing the ruler can affect the performance.
--
-- I don't add this directly to 'RulerUtil.local_meter' because that would make
-- it be in Cmd and IO.
local_meter :: BlockId -> (Meter.Meter -> Meter.Meter) -> Cmd.CmdL ()
local_meter block_id f = do
    RulerUtil.local_meter block_id f
    Cmd.invalidate_performances

modify_block :: BlockId -> (Ruler.Ruler -> Ruler.Ruler) -> Cmd.CmdL ()
modify_block block_id f = do
    RulerUtil.modify_block block_id f
    Cmd.invalidate_performances

-- * extract

extract :: Cmd.CmdL ()
extract = do
    (block_id, _, track_id, _) <- Selection.get_insert
    extract_from block_id track_id

-- | Extract the meter marklists from the sub-blocks called on the given
-- track, concatenate them, and replace the current meter with it.
extract_from :: BlockId -> TrackId -> Cmd.CmdL ()
extract_from block_id track_id = do
    subs <- extract_calls track_id
    ruler_ids <- mapM State.ruler_of [bid | (_, _, bid) <- subs]
    meters <- mapM RulerUtil.get_meter ruler_ids
    let all_meters = concat [Meter.scale dur meter
            | ((_start, dur, _), meter) <- zip subs meters]
    local_meter block_id (const all_meters)

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
