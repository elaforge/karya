-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds to do with \"refactoring\".  This basically means fancy
-- copy-paste-like operations.
module Cmd.Factor where
import qualified Data.Text as Text

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Meter as Meter
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.RulerUtil as RulerUtil
import qualified Cmd.Selection as Selection

import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Eval as Eval
import Global
import Types


-- | Split the block at the time of the current selection, and create a new
-- block and view with everything afterwards.  The old and new blocks are
-- renamed with @-1@ and @-2@ suffixes, respectively.  The old block is renamed
-- for symmetry with the new one, but mostly because it's changed duration, so
-- previous calls are probably no longer valid.
split_time :: Cmd.M m => m BlockId -- ^ BlockId of new block
split_time = do
    (block_id, _, _, pos) <- Selection.get_insert
    let (from_block, to_block) = split_names block_id
    to_block_id <- split_time_at block_id pos to_block
    Create.view to_block_id
    Create.rename_block block_id from_block
    return to_block_id

-- | Create a new block from template, then copy over all the events below the
-- given time.  Clear the source track, and trim events that overlap the split
-- point.  Modify the ruler (locally!) in the old and new blocks.
split_time_at :: State.M m => BlockId -> ScoreTime -> Id.Id -> m BlockId
split_time_at from_block_id pos block_name = do
    tracks <- State.tracknums_of from_block_id
    -- Copy over the new events.
    track_events <- forM tracks $ \(track_id, tracknum) -> do
        events <- Events.at_after pos <$> State.get_events track_id
        let shifted = map (Event.move (subtract pos)) events
        return (tracknum, shifted)
    -- Trim the old events.
    forM_ tracks $ \(track_id, _) -> do
        events <- fst . Events.split pos <$> State.get_events track_id
        let clipped = Events.from_list $ Events.clip False pos $
                Events.ascending events
        State.modify_events track_id (const clipped)
    -- Create new block.
    to_block_id <- Create.named_block_from_template False from_block_id
        block_name
    forM_ track_events $ \(tracknum, events) -> do
        track_id <- State.get_event_track_at to_block_id tracknum
        State.insert_events track_id events
    -- Trim rulers on each.
    let dur = Meter.time_to_duration pos
    local_block from_block_id $ Meter.clip 0 dur
    local_block to_block_id $ Meter.delete 0 dur
    return to_block_id

split_names :: BlockId -> (Id.Id, Id.Id)
split_names block_id =
    (Id.modify_name (<>"-1") id, Id.modify_name (<>"-2") id)
    where id = Id.unpack_id block_id

-- | Put all tracks with a after the selection into a new block.
--
-- Unlike 'split_time' I don't rename the source block, because the length
-- unchanged.
split_track :: Cmd.M m => m BlockId
split_track = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    to_block_id <- split_track_at block_id tracknum
        (snd (split_names block_id))
    Create.view to_block_id
    return to_block_id

split_track_at :: State.M m => BlockId -> TrackNum -> Id.Id -> m BlockId
split_track_at from_block_id split_at block_name = do
    to_block_id <- Create.named_block block_name
        =<< State.ruler_of from_block_id
    skeleton <- State.get_skeleton from_block_id
    -- Move tracks.
    tracks <- zip [0..] . Block.block_tracks <$> State.get_block from_block_id
    forM_ (dropWhile ((<split_at) . fst) tracks) $ \(tracknum, track) ->
        State.insert_track to_block_id (tracknum - split_at + 1) track
    forM_ (takeWhile ((>=split_at) . fst) (reverse tracks)) $
        \(tracknum, _) -> State.remove_track from_block_id tracknum
    -- Copy over the skeleton.
    State.set_skeleton to_block_id $ Skeleton.make
        [ (from-split_at + 1, to-split_at + 1)
        | (from, to) <- Skeleton.flatten skeleton
        , from >= split_at && to >= split_at
        ]
    return to_block_id

-- | Copy the selection into a new block, and replace it with a call to that
-- block.
selection :: Cmd.M m => Id.Id -> m BlockId
selection name = do
    block_id <- selection_ False name
    Create.view block_id
    return block_id

-- | Copy the selection to a relative block, and replace it with a relative
-- block call.
selection_relative :: Cmd.M m => Id.Id -> m BlockId
selection_relative name = do
    block_id <- selection_ True name
    Create.view block_id
    return block_id

-- | Same as 'selection_relative' because I always type the wrong one.
relative_selection :: Cmd.M m => Id.Id -> m BlockId
relative_selection = selection_relative

-- | Create a number of alternate versions of the selection, and insert
-- an @alt@ call.
selection_alts :: Cmd.M m => Bool -> Int -> Id.Id -> m [BlockId]
selection_alts relative alts name
    | alts <= 0 = return []
    | otherwise = do
        alt1 <- selection_ relative $ Id.modify_name (<>"1") name
        (block_id, _, track_ids, start, end) <- Selection.tracks
        altn <- forM [2..alts] $ \n ->
            Create.named_block_from_template True alt1 $
                alt_name block_id (Id.modify_name (<> showt n) name)
        let alts = alt1 : altn
        mapM_ Create.view alts
        let call = Text.unwords $
                "alt" : map (Eval.block_id_to_call relative block_id) alts
        whenJust (Seq.head track_ids) $ \track_id ->
            State.insert_event track_id $ Event.event start (end-start) call
        return alts
    where
    alt_name block_id name =
        if relative then make_relative block_id name else name

-- | Copy the selection into a new block, and replace it with a call to that
-- block.
selection_ :: Cmd.M m => Bool -- ^ create relative block call
    -> Id.Id -> m BlockId
selection_ relative name = do
    (block_id, tracknums, track_ids, start, end) <- Selection.tracks
    name <- return $ if relative
        then make_relative block_id name else name
    selection_at relative name block_id tracknums track_ids start end

make_relative :: BlockId -> Id.Id -> Id.Id
make_relative caller name =
    Id.set_name (Eval.make_relative caller (Id.id_name name)) name

selection_at :: State.M m => Bool -> Id.Id -> BlockId -> [TrackNum]
    -> [TrackId] -> TrackTime -> TrackTime -> m BlockId
selection_at relative name block_id tracknums track_ids start end = do
    ruler_id <- State.block_ruler block_id
    to_block_id <- Create.named_block name ruler_id
    forM_ (zip [1..] track_ids) $ \(tracknum, track_id) -> do
        title <- State.get_track_title track_id
        events <- Events.in_range (Events.range Event.Positive start end) <$>
            State.get_events track_id
        -- Shift the events back to start at 0.
        Create.track to_block_id tracknum title $
            Events.map_events (Event.move (subtract start)) events
    clipped_skeleton block_id to_block_id tracknums
    -- Clear selected range and put in a call to the new block.
    Edit.clear_range track_ids (Events.Positive start end)
    whenJust (Seq.head track_ids) $ \track_id ->
        State.insert_event track_id $ Event.event start (end-start)
            (Eval.block_id_to_call relative block_id to_block_id)
    -- It's easier to create all the tracks and then delete the empty ones.
    -- If I tried to just not create those tracks then 'clipped_skeleton' would
    -- have to get more complicated.
    delete_empty_tracks to_block_id
    -- Create a clipped ruler.
    local_block to_block_id $
        Meter.clip (Meter.time_to_duration start) (Meter.time_to_duration end)
    return to_block_id

-- | Update relative calls on a block to a new parent.
rebase_relative_calls :: State.M m =>
    Bool -- ^ if true, copy the call, otherwise rename it
    -> BlockId -> BlockId -> m ()
rebase_relative_calls copy from to = do
    let ns = Id.ident_namespace from
    track_ids <- Block.block_track_ids <$> State.get_block to
    called <- Seq.unique . mapMaybe (resolve_relative_call ns from)
        . concat <$> mapM get_block_calls track_ids
    forM_ (zip called (map (rebase_call from) called)) $ \(old, new) ->
        (if copy then Create.copy_block else Create.rename_block) old new

-- | Move a relative call from one caller to another.
--
-- ns1/caller ns2/old.sub -> ns1/caller.sub
rebase_call :: BlockId -> BlockId -> Id.Id
rebase_call caller block_id = Id.id ns name
    where
    (ns, caller_name) = Id.un_id (Id.unpack_id caller)
    -- old.bar -> caller.bar
    -- a.b.c -> a.caller.c
    -- root.old.sub -> root.caller.sub
    old_name = Id.ident_name block_id
    name
        | Text.count "." old_name > 0 =
            caller_name <> Text.dropWhile (/='.') old_name
        | otherwise = old_name

get_block_calls :: State.M m => TrackId -> m [BaseTypes.CallId]
get_block_calls track_id = do
    events <- Events.ascending <$> State.get_events track_id
    return $ map BaseTypes.Symbol $
        concatMap (NoteTrack.possible_block_calls . Event.text) events

resolve_relative_call :: Id.Namespace -> BlockId -> BaseTypes.CallId
    -> Maybe BlockId
resolve_relative_call ns caller sym
    | Eval.is_relative sym = Eval.call_to_block_id ns (Just caller) sym
    | otherwise = Nothing

-- | If there's a point selection, create a new empty block based on the
-- current one.  If the selection has time, then the new block will have only
-- the selected tracks with a ruler clipped to the selected range.
block_from_template :: Cmd.M m => m ()
block_from_template = do
    (_, sel) <- Selection.get
    if Sel.is_point sel
        then void $ Create.view =<< Create.block_from_template False
            =<< Cmd.get_focused_block
        else void block_template_from_selection

delete_empty_tracks :: State.M m => BlockId -> m ()
delete_empty_tracks block_id = do
    track_ids <- filterM (fmap Events.null . State.get_events)
        =<< State.track_ids_of block_id
    mapM_ State.destroy_track track_ids

-- * named block

block_template_from_selection :: Cmd.M m => m BlockId
block_template_from_selection =
    Selection.tracks >>= \(block_id, _, track_ids, start, end) -> do
        to_block_id <- block_template block_id track_ids start end
        Create.view to_block_id
        return to_block_id

-- | Create a new block with the given tracks and ruler clipped to the given
-- range.
block_template :: State.M m => BlockId -> [TrackId] -> TrackTime -> TrackTime
    -> m BlockId
block_template block_id track_ids start end = do
    to_block_id <- Create.block =<< State.block_ruler block_id
    forM_ (zip [1..] track_ids) $ \(tracknum, track_id) -> do
        title <- State.get_track_title track_id
        Create.track to_block_id tracknum title mempty
    -- Create skeleton.
    clipped_skeleton block_id to_block_id
        =<< mapM (State.get_tracknum_of block_id) track_ids
    -- Create a clipped ruler.
    local_block to_block_id $
        Meter.clip (Meter.time_to_duration start) (Meter.time_to_duration end)
    return to_block_id

clipped_skeleton :: State.M m => BlockId -> BlockId -> [TrackNum] -> m ()
clipped_skeleton from_block to_block tracknums =
    case (Seq.minimum tracknums, Seq.maximum tracknums) of
        (Just low, Just high) -> do
            edges <- Skeleton.flatten <$> State.get_skeleton from_block
            State.set_skeleton to_block $ Skeleton.make
                [ (from-low + 1, to-low + 1) | (from, to) <- edges
                , Num.inRange low (high+1) from, Num.inRange low (high+1) to
                ]
        _ -> return ()

-- * order block

-- | Create a new block containing calls to the given BlockIds.
order_block :: Cmd.M m => Id.Id -> [BlockId] -> m BlockId
order_block name block_ids = do
    block_id <- Create.named_block name State.no_ruler
    order_track block_id block_ids
    Create.view block_id
    return block_id

-- | Append a track to the given block with calls to the given BlockIds.  The
-- calling track will have a 1:1 time relationship with the calls, which is
-- useful for lilypond derivation since it only understands 1:1.  Also
-- modify the ruler to be the concatenation of the rulers of the sub-blocks.
order_track :: State.M m => BlockId -> [BlockId] -> m TrackId
order_track block_id sub_blocks = do
    ruler_ids <- mapM State.ruler_of sub_blocks
    meters <- mapM RulerUtil.get_meter ruler_ids
    let durs = map Meter.time_end meters
        starts = scanl (+) 0 durs
        events = [Event.event start dur (block_id_to_call block_id)
            | (start, dur, block_id) <- zip3 starts durs sub_blocks]
    local_block block_id $ const $ mconcat meters
    Create.track block_id 9999 ">" (Events.from_list events)

block_id_to_call :: BlockId -> Text
block_id_to_call = Id.ident_name

-- * util

local_block :: State.M m => BlockId
    -> (Meter.LabeledMeter -> Meter.LabeledMeter) -> m [RulerId]
local_block block_id = RulerUtil.local_block block_id . Meter.modify_meter
