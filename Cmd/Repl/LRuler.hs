-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Work with rulers and meters.  A meter is a marklist on a ruler named
    'Ruler.meter', and is used by "Cmd.TimeStep" to align things.  By
    convention the meter has regular subdivisions, with 'Ruler.Rank's that
    correspond roughly to timestep durations (e.g. whole, half, quarter notes).
    The ruler marks are numbered, and this module, with the support of
    "Cmd.RulerUtil", lets you modify the meter in a higher level way and takes
    care of renumbering the labels so e.g. measure numbers always count up even
    if you double the length of the meter.

    Ultimately this is necessary because I want to manipulate rulers as a high
    level 'Meter.Meter' or 'Meter.LabeledMeter', but 'Ruler.Marklist' has lost
    the meter's structure.  That in turn is because different kinds of meters,
    talams, gong cycles, etc. have different structures and I didn't think
    I could come up with a single high level data structure that fit them all
    and still allowed generic manipulation.

    Many functions emit a 'Modify'.  If defaults to 'RulerUtil.Section' scope,
    but you can change it to work on selected tracks with 'tracks' or all
    rulers in the block with 'block'.  Then, the 'modify' function will
    destructively modify selected rulers, while the 'local' function will
    modify via copy-on-write, so that other blocks or tracks are unaffected.

    Examples:

    - Give the current block 6 sections of standard 4/4 meter, with 4 measures
    per section, where each measure gets 1t:

        > LRuler.local $ LRuler.ruler $ LRuler.measures 1 Meters.m44 6 4

    - Or if you want each quarter note to get 1t, and 8 sections with
    4 measures per section:

        > LRuler.local $ LRuler.ruler $ LRuler.measures 4 Meters.m44 8 4

    - Or put the selection at the where the 4 meters should end, then:

        > LRuler.local $ LRuler.ruler $
        >       LRuler.fit_to_selection LRuler.config Meter.m44

    - Make the last measure 5/4 by selecting a quarter note and running
      @LRuler.append@.

    - TODO make a middle measure 5/4?

    - Set a block to 4 sections of 4 avartanams of adi talam, then select
    tracks and set them to chatusram-tisram:

        > LRuler.modify $ LRuler.ruler $ Tala.ruler $ Tala.adi 4
        > LRuler.local $ LRuler.tracks $ LRuler.ruler $ LTala.chatis 4 4 4
-}
module Cmd.Repl.LRuler where
import Prelude hiding (concat)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Block as Block
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
import qualified Cmd.Meters as Meters
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.PlayUtil as PlayUtil
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
blocks_of :: State.M m => RulerId -> m [BlockId]
blocks_of = fmap (map fst) . State.blocks_with_ruler_id

-- | Set the rulers on a block to the given RulerId.
set_ruler_id :: State.M m => RulerId -> BlockId -> m ()
set_ruler_id ruler_id block_id = do
    old <- State.block_ruler block_id
    State.replace_ruler_id block_id old ruler_id

-- | Copy the ruler of the given block to the current one.
copy :: Cmd.M m => BlockId -> m ()
copy other_block = do
    other_ruler <- State.ruler_of other_block
    this_block <- Cmd.get_focused_block
    this_ruler <- State.block_ruler this_block
    State.replace_ruler_id this_block this_ruler other_ruler

-- | Set the ruler of the tracks in the given scope.
set :: State.M m => RulerId -> BlockId -> RulerUtil.Scope -> m ()
set ruler_id block_id scope = do
    ruler_ids <- case scope of
        RulerUtil.Block -> do
            count <- State.track_count block_id
            return $ replicate count (Just ruler_id)
        RulerUtil.Section tracknum -> replace_tracknums
            =<< map fst <$> RulerUtil.get_section block_id tracknum
        RulerUtil.Tracks tracknums -> replace_tracknums tracknums
    State.set_ruler_ids block_id ruler_ids
    where
    replace_tracknums tracknums = do
        old <- map Block.ruler_id_of . Block.block_tracklike_ids <$>
                State.get_block block_id
        let replace tracknum old_ruler
                | tracknum `elem` tracknums = Just ruler_id
                | otherwise = old_ruler
        return $ zipWith replace [0..] old

-- | Replace the ruler.
ruler :: Cmd.M m => Ruler.Ruler -> m Modify
ruler r = do
    (block_id, tracknum) <- get_block_track
    return $ make_modify block_id tracknum $ const (Right r)

-- | Modify all rulers.
modify_rulers :: Cmd.M m => (Ruler.Ruler -> Ruler.Ruler) -> m ()
modify_rulers modify = do
    ruler_ids <- State.all_ruler_ids
    forM_ ruler_ids $ \ruler_id ->
        State.modify_ruler ruler_id (Right . modify)

-- | Replace all occurrences of one RulerId with another.
replace_ruler_id :: State.M m => RulerId -> RulerId -> m ()
replace_ruler_id old new = do
    blocks <- State.blocks_with_ruler_id old
    forM_ (map fst blocks) $ \block_id ->
        State.replace_ruler_id block_id old new

-- * query

get_meter :: State.M m => BlockId -> m Meter.LabeledMeter
get_meter block_id =
    Meter.ruler_meter <$> (State.get_ruler =<< State.ruler_of block_id)

get_marks :: State.M m => BlockId -> m [Ruler.PosMark]
get_marks block_id =
    Ruler.ascending 0 . snd . Ruler.get_marklist Ruler.meter <$>
        (State.get_ruler =<< State.ruler_of block_id)

-- | Ruler of the track under the selection.
selected :: Cmd.M m => m RulerId
selected = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    Cmd.require "no ruler" =<< State.ruler_track_at block_id tracknum

-- * Modify

-- | Double the meter of the current block. You can then trim it down to size.
double :: Cmd.M m => m Modify
double = modify_selected $ \meter -> Seq.rdrop 1 meter <> meter
    -- The final 0 duration mark should be replaced by the first mark.

-- | Clip the meter to end at the selection.
clip :: Cmd.M m => m Modify
clip = do
    (_, _, _, pos) <- Selection.get_insert
    modify_selected $ Meter.clip 0 (Meter.time_to_duration pos)

-- | Copy the meter under the selection and append it to the end of the ruler.
append :: Cmd.M m => m Modify
append = do
    (start, end) <- Selection.range
    modify_selected $ \meter ->
        meter <> Meter.clip (Meter.time_to_duration start)
            (Meter.time_to_duration end) meter

-- | Append another ruler to this one.
append_ruler_id :: Cmd.M m => RulerId -> m Modify
append_ruler_id ruler_id = do
    other <- Meter.ruler_meter <$> State.get_ruler ruler_id
    modify_selected $ (<> other) . Seq.rdrop 1

-- | Remove the selected range of the ruler and shift the rest up.
delete :: Cmd.M m => m Modify
delete = do
    (start, end) <- Selection.range
    modify_selected $ Meter.delete
        (Meter.time_to_duration start) (Meter.time_to_duration end)

-- | Replace the selected region with another marklist.
replace :: Cmd.M m => Meter.LabeledMeter -> m Modify
replace insert = do
    (start, end) <- Selection.range
    modify_selected $ replace_range start end insert

-- | Replace the selected region with another marklist.
replace_range :: TrackTime -> TrackTime -> Meter.LabeledMeter
    -> Meter.LabeledMeter -> Meter.LabeledMeter
replace_range start end insert meter =
        before <> Meter.take_before (end - start) insert <> after
        where
        before = Meter.take_before start meter
        after = Meter.drop_until end meter

-- | Strip out ranks below a certain value, for the whole block.  Larger scale
-- blocks don't need the fine resolution and can wind up with huge rulers.
strip_ranks :: Cmd.M m => Meter.RankName -> m Modify
strip_ranks max_rank =
    modify_selected $ Meter.strip_ranks (Meter.name_to_rank max_rank)

-- | Set the ruler to a number of measures of the given meter, where each
-- measure gets 1t:
--
-- > local $ measures Meters.m44 4 4
-- > modify $ measures Meters.m34 4 8
measures :: Meter.AbstractMeter -> Int -- ^ sections
    -> Int -- ^ measures per section
    -> Ruler.Ruler
measures = make_measures config 1

make_measures :: Meter.MeterConfig -> TrackTime -- ^ duration of one measure
    -> Meter.AbstractMeter
    -> Int -- ^ sections
    -> Int -- ^ measures per section
    -> Ruler.Ruler
make_measures config dur meter sections measures =
    fit_ruler config (dur * fromIntegral (measures * sections))
        (replicate sections (Meter.repeat measures meter))

-- | Create a meter ruler fitted to the end of the last event on the block.
fit_to_end :: State.M m => Meter.MeterConfig -> [Meter.AbstractMeter]
    -> BlockId -> m Ruler.Ruler
fit_to_end config meter block_id = do
    end <- State.block_event_end block_id
    return $ fit_ruler config end meter

config :: Meter.MeterConfig
config = Meter.default_config

fit_to_selection :: Cmd.M m => Meter.MeterConfig -> [Meter.AbstractMeter]
    -> m Ruler.Ruler
fit_to_selection config meter = do
    (_, _, _, pos) <- Selection.get_insert
    return $ fit_ruler config pos meter

-- | Make a ruler fit in the given duration.
fit_ruler :: Meter.MeterConfig -> ScoreTime -> [Meter.AbstractMeter]
    -> Ruler.Ruler
fit_ruler config dur meters =
    (if Meter.meter_from0 config then Meters.ruler0 else Meters.ruler) $
    Meter.meter_marklist config $
    Meter.fit_meter (Meter.time_to_duration dur) meters

-- | Replace the meter with the concatenation of the rulers of the given
-- blocks.  This is like 'extract' except it doesn't infer the blocks from the
-- calls and doesn't scale the extracted rulers.
concat :: Cmd.M m => [BlockId] -> m Modify
concat block_ids = do
    ruler_ids <- mapM State.ruler_of block_ids
    -- Strip the last 0-dur mark off of each meter before concatenating.
    meters <- map (Seq.rdrop 1) <$> mapM RulerUtil.get_meter ruler_ids
    modify_selected $ const $ mconcat meters ++ [final_mark]

-- * extract

-- | Extract the meter marklists from the sub-blocks called on the given
-- track, concatenate them, and replace the current meter with it.
extract :: Cmd.M m => m Modify
extract = do
    (block_id, tracknum, track_id, _) <- Selection.get_insert
    all_meters <- extract_meters block_id track_id
    return $ make_modify block_id tracknum $
        Meter.modify_meter (const all_meters)

extract_meters :: Cmd.M m => BlockId -> TrackId -> m Meter.LabeledMeter
extract_meters block_id track_id = do
    subs <- extract_calls block_id track_id
    ruler_ids <- mapM State.ruler_of [bid | (_, _, bid) <- subs]
    -- Strip the last 0-dur mark off of each meter before concatenating.
    meters <- map (Seq.rdrop 1) <$> mapM RulerUtil.get_meter ruler_ids
    return $ mconcat $
        [ Meter.scale (Meter.time_to_duration dur) meter
        | ((_start, dur, _), meter) <- zip subs meters
        ] ++ [[final_mark]]

final_mark :: Meter.LabeledMark
final_mark = Meter.LabeledMark 0 0 ""

extract_calls :: State.M m => BlockId -> TrackId
    -> m [(ScoreTime, ScoreTime, BlockId)]
extract_calls block_id track_id =
    mapMaybeM extract =<< Events.ascending . Track.track_events <$>
        State.get_track track_id
    where
    extract event = fmap (range event) <$>
        NoteTrack.block_call (Just block_id) (Event.event_text event)
    range event block_id = (Event.start event, Event.duration event, block_id)

-- * modify

-- | Modify only the selected tracks, or the entire block.  'Section' is the
-- default.
tracks :: Cmd.M m => m Modify -> m Modify
tracks modify = do
    modify <- modify
    (_, tracknums, _, _, _) <- Selection.tracks
    return $ modify { m_scope = RulerUtil.Tracks tracknums }

-- | Modify the entire block.
block :: Cmd.M m => m Modify -> m Modify
block modify = do
    modify <- modify
    return $ modify { m_scope = RulerUtil.Block }

-- | Enough information to modify a ruler.
--
-- TODO I could also include entire block, and then add_cue etc. could use it,
-- in addition to being able to clip the entire block.
data Modify = Modify {
    m_block_id :: !BlockId
    , m_scope :: !RulerUtil.Scope
    , m_modify :: !Meter.ModifyRuler
    }

modify_selected :: Cmd.M m => (Meter.LabeledMeter -> Meter.LabeledMeter)
    -> m Modify
modify_selected modify = do
    (block_id, tracknum) <- get_block_track
    return $ make_modify block_id tracknum (Meter.modify_meter modify)

make_modify :: BlockId -> TrackNum -> Meter.ModifyRuler -> Modify
make_modify block_id tracknum = Modify block_id (RulerUtil.Section tracknum)

get_block_track :: Cmd.M m => m (BlockId, TrackNum)
get_block_track = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    return (block_id, tracknum)

local :: Cmd.M m => m Modify -> m [RulerId]
local = (local_m =<<)

modify :: Cmd.M m => m Modify -> m ()
modify = (modify_m =<<)

-- | Modify a ruler or rulers, making a copy if they're shared with another
-- block.
local_m :: Cmd.M m => Modify -> m [RulerId]
local_m (Modify block_id scope modify) = do
    PlayUtil.clear_caches -- The ruler can affect derivation.
    RulerUtil.local scope block_id modify

-- | Modify the ruler on the focused block.  Other blocks with the same ruler
-- will also be modified.
modify_m :: Cmd.M m => Modify -> m ()
modify_m (Modify block_id scope modify) = do
    PlayUtil.clear_caches
    RulerUtil.modify scope block_id modify


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
    RulerUtil.modify_block block_id $ Right . Ruler.remove_marklist cue

add_cue_at :: BlockId -> ScoreTime -> Text -> Cmd.CmdL ()
add_cue_at block_id pos text = RulerUtil.modify_block block_id $
    Right . Ruler.modify_marklist cue
        (const (Ruler.insert_mark pos (cue_mark text)))

cue_mark :: Text -> Ruler.Mark
cue_mark text = Ruler.Mark 0 2 Color.black text 0 0


-- * colors

-- | Used to adjust mark colors interactively.
reset_colors :: Cmd.CmdL ()
reset_colors = do
    block_id <- Cmd.get_focused_block
    ruler_id <- State.ruler_of block_id
    State.modify_ruler ruler_id (Right . set_colors meter_ranks)

set_colors :: [(Color.Color, Meter.MarkWidth, Int)] -> Ruler.Ruler
    -> Ruler.Ruler
set_colors ranks =
    Ruler.modify_marklist Ruler.meter
        (const $ Ruler.marklist . map (second set) . Ruler.ascending 0)
    where
    set mark = case Seq.at ranks (Ruler.mark_rank mark) of
        Nothing -> mark
        Just (color, width, _) -> mark
            { Ruler.mark_color = color
            , Ruler.mark_width = width
            }

meter_ranks :: [(Color.Color, Meter.MarkWidth, Int)]
meter_ranks =
    [ (a8 0.0 0.0 0.0, 3, 8)    -- section
    , (a8 0.2 0.1 0.0, 2, 8)    -- measure / whole

    , (a8 1.0 0.4 0.2, 2, 8)    -- half
    , (a5 1.0 0.4 0.2, 2, 8)    -- quarter

    , (a8 1.0 0.4 0.9, 1, 8)    -- 8th
    , (a5 1.0 0.4 0.9, 1, 8)    -- 16th

    , (a5 0.1 0.5 0.1, 1, 8)    -- 32nd
    , (a2 0.1 0.5 0.1, 1, 8)    -- 64th

    , (a5 0.0 0.0 0.0, 1, 8)    -- 128th
    , (a2 0.0 0.0 0.0, 1, 8)    -- 256th
    ]
    where
    a2 = alpha 0.2
    a5 = alpha 0.5
    a8 = alpha 0.8
    alpha a r g b = Color.rgba r g b a
