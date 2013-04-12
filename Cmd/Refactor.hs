-- | Cmds to do with \"refactoring\".  This basically means fancy
-- copy-paste-like operations.
module Cmd.Refactor where
import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Meter as Meter
import qualified Cmd.RulerUtil as RulerUtil
import qualified Cmd.Selection as Selection

import Types


-- | Split the block at the time of the current selection, and create a new
-- block and view with everything afterwards.  The old and new blocks are
-- renamed with @-1@ and @-2@ suffixes, respectively.  The old block is renamed
-- for symmetry with the new one, but mostly because it's changed duration, so
-- previous calls are probably no longer valid.
split_time :: (Cmd.M m) => m BlockId -- ^ BlockId of new block
split_time = do
    (block_id, _, _, pos) <- Selection.get_insert
    let (from_block, to_block) = split_names block_id
    to_block_id <- split_time_at block_id pos to_block
    Create.view to_block_id
    new_from <- State.read_id from_block
    Create.rename_block block_id new_from
    return to_block_id

-- | Create a new block from template, then copy over all the events below the
-- given time.  Clear the source track, and trim events that overlap the split
-- point.  Modify the ruler (locally!) in the old and new blocks.
split_time_at :: (State.M m) => BlockId -> ScoreTime -> String -> m BlockId
split_time_at from_block_id pos block_name = do
    tracks <- State.tracknums_of from_block_id
    -- Copy over the new events.
    track_events <- forM tracks $ \(track_id, tracknum) -> do
        events <- snd . Events.split pos . Track.track_events <$>
            State.get_track track_id
        let shifted = map (Event.move (subtract pos)) events
        return (tracknum, shifted)
    -- Trim the old events.
    forM_ tracks $ \(track_id, _) -> do
        events <- fst . Events.split_at pos . Track.track_events <$>
            State.get_track track_id
        let clipped = Events.from_list $ Events.clip pos $
                Events.ascending events
        State.modify_events track_id (const clipped)
    -- Create new block.
    to_block_id <- Create.named_block_from_template from_block_id block_name
    forM_ track_events $ \(tracknum, events) -> do
        track_id <- State.get_event_track_at to_block_id tracknum
        State.insert_events track_id events
    -- Trim rulers on each.
    RulerUtil.local_meter from_block_id $ Meter.clip 0 pos
    RulerUtil.local_meter to_block_id $ Meter.remove 0 pos
    return to_block_id

split_names :: BlockId -> (String, String)
split_names block_id = (name ++ "-1", name ++ "-2")
    where name = Id.ident_name block_id

-- | Put all tracks with a after the selection into a new block.
--
-- Unlike 'split_time' I don't rename the source block, because the length
-- unchanged.
split_track :: (Cmd.M m) => m BlockId
split_track = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    to_block_id <- split_track_at block_id tracknum
        (snd (split_names block_id))
    Create.view to_block_id
    return to_block_id

split_track_at :: (State.M m) => BlockId -> TrackNum -> String -> m BlockId
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
selection :: (Cmd.M m) => String -> m BlockId
selection name = do
    (block_id, tracknums, track_ids, start, end) <- Selection.tracks
    to_block_id <- selection_at (Just name) block_id tracknums track_ids
        start end
    Create.view to_block_id
    return to_block_id

selection_at :: (State.M m) => Maybe String -> BlockId -> [TrackNum]
    -> [TrackId] -> TrackTime -> TrackTime -> m BlockId
selection_at maybe_name block_id tracknums track_ids start end = do
    ruler_id <- State.block_ruler block_id
    to_block_id <- maybe Create.block Create.named_block maybe_name ruler_id
    forM_ (zip [1..] track_ids) $ \(tracknum, track_id) -> do
        title <- State.get_track_title track_id
        events <- Events.in_range_point start end . Track.track_events <$>
            State.get_track track_id
        -- Shift the events back to start at 0.
        Create.track to_block_id tracknum title $
            Events.map_events (Event.move (subtract start)) events
    edges <- Skeleton.flatten <$> State.get_skeleton block_id
    case (Seq.minimum tracknums, Seq.maximum tracknums) of
        (Just low, Just high) -> State.set_skeleton to_block_id $ Skeleton.make
            [ (from-low, to-low) | (from, to) <- edges
            , Num.in_range low high from, Num.in_range low high to
            ]
        _ -> return ()
    -- Clear selected range and put in a call to the new block.
    Edit.clear_range track_ids start end
    when_just (Seq.head track_ids) $ \track_id ->
        State.insert_event track_id $ Event.event start (end-start)
            (Id.ident_name to_block_id)
    -- Create a clipped ruler.
    RulerUtil.local_meter to_block_id $ Meter.clip start end
    return to_block_id
