-- | Cmds for track level operations.
module Cmd.Lang.LTrack where
import Util.Control
import qualified Util.ParseBs as ParseBs
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection

import qualified Derive.TrackInfo as TrackInfo
import Types


-- | Remove tracks with no events from the given block.
remove_empty :: BlockId -> Cmd.CmdL ()
remove_empty block_id = do
    track_ids <- Block.block_track_ids <$> State.get_block block_id
    mapM_ State.destroy_track =<< filterM is_empty track_ids
    where
    is_empty = fmap ((==Events.empty) . Track.track_events)
        . State.get_track

remove_all_empty :: Cmd.CmdL ()
remove_all_empty = mapM_ remove_empty =<< State.all_block_ids

-- | Transform all track titles.
map_titles :: (String -> String) -> Cmd.CmdL ()
map_titles f = do
    bids <- State.all_block_ids
    mapM_ (flip map_block_titles f) bids

-- Should this go in Ui.Transform?
-- TODO should map 'x | abc' to 'y | abc'
-- And 'mul x' -> 'mul y'
--
-- Use Cmd.Info and do replace instead of map.
map_block_titles :: BlockId -> (String -> String) -> Cmd.CmdL ()
map_block_titles block_id f = do
    tids <- map State.track_id <$> State.get_track_info block_id
    mapM_ (flip State.modify_track_title f) tids

replace x y val
    | val == x = y
    | otherwise = val

-- * events

events :: TrackId -> ScoreTime -> ScoreTime -> Cmd.CmdL [Events.PosEvent]
events track_id start end = do
    track <- State.get_track track_id
    return $ (Events.ascending
        . Events.in_range start end . Track.track_events) track

-- * strip controls

-- | Strip repeated controls, e.g. @.5@ followed by @.5@.
strip_block_controls :: BlockId -> Cmd.CmdL ()
strip_block_controls block_id =
    ModifyEvents.block_tracks block_id strip_track_controls

strip_track_controls :: (Cmd.M m) => ModifyEvents.Track m
strip_track_controls track_id events = do
    title <- State.get_track_title track_id
    return $ if TrackInfo.is_signal_track title
        then Just (strip_controls events)
        else Nothing

strip_controls :: [Events.PosEvent] -> [Events.PosEvent]
strip_controls = map snd . filter same . Seq.zip_prev
    where
    same (Nothing, _) = True
    same (Just (_, prev), (_, cur)) =
        not $ is_set (str prev) && str prev == str cur
    str = Event.event_bs
    is_set = right . ParseBs.parse_all ParseBs.p_float
    right (Right _) = True -- why isn't this in Data.Either?
    right (Left _) = False

-- * signal render

filled :: Cmd.CmdL ()
filled = do
    (_, _, track_id, _) <- Selection.get_insert
    State.set_render_style Track.Filled track_id

line :: Cmd.CmdL ()
line = do
    (_, _, track_id, _) <- Selection.get_insert
    State.set_render_style Track.Line track_id

no_render :: Cmd.CmdL ()
no_render = do
    (_, _, track_id, _) <- Selection.get_insert
    State.set_render_style Track.NoRender track_id
