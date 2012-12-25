{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Cmds for track level operations.
module Cmd.Lang.LTrack where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.Create as Create
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Selection as Selection

import qualified Derive.ParseBs
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Types


gc :: Cmd.CmdL ()
gc = mapM_ State.destroy_track . Set.elems =<< Create.orphan_tracks

-- | Tracks that don't appear in any block.
orphans :: Cmd.CmdL [TrackId]
orphans = Set.elems <$> Create.orphan_tracks

-- | Every track along with how many times it appears in a block.
refs :: Cmd.CmdL [(TrackId, Int)]
refs = do
    st <- State.get
    let tids = concatMap Block.block_track_ids
            (Map.elems (State.state_blocks st))
        insert fm tid = Map.insertWith (+) tid 1 fm
        ref_map = List.foldl' insert Map.empty tids
    return [(tid, Map.findWithDefault 0 tid ref_map)
        | tid <- Map.keys (State.state_tracks st)]

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

map_widths :: (String -> Bool) -> (Types.Width -> Types.Width) -> Cmd.CmdL ()
map_widths wanted f = do
    block_ids <- State.all_block_ids
    forM_ block_ids $ \block_id -> do
        tracknums <- map State.track_tracknum
            . filter (wanted . State.track_title) <$>
                TrackTree.tracks_of block_id
        widths <- map Block.track_width <$>
            mapM (State.get_block_track_at block_id) tracknums
        zipWithM_ (State.set_track_width block_id)
            tracknums (map f widths)

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
    tids <- map State.track_id <$> TrackTree.tracks_of block_id
    mapM_ (flip State.modify_track_title f) tids

-- * manipulation

-- | Duplicate a track from one block to another.  The underlying track is
-- the same, so edits in one of its occurrances will be reflected in all of its
-- blocks.
duplicate :: (State.M m) => BlockId -> TrackNum -> BlockId -> TrackNum -> m ()
duplicate source_block source_tracknum dest_block dest_tracknum = do
    track <- State.get_block_track_at source_block source_tracknum
    State.insert_track dest_block dest_tracknum track

-- * control tracks

map_control_val :: String -> (Signal.Y -> Signal.Y) -> Cmd.CmdL ()
map_control_val name f = ModifyEvents.selection $
    ModifyEvents.tracks_named (==name) $ ModifyEvents.text $ \text ->
        fromMaybe text (ControlTrack.modify_val f text)

score_to_hex :: Cmd.CmdL ()
score_to_hex = ModifyEvents.all_blocks $
    ModifyEvents.tracks_named TrackInfo.is_signal_track $
        ModifyEvents.text to_hex

block_to_hex :: BlockId -> Cmd.CmdL ()
block_to_hex block_id = ModifyEvents.block block_id $
    ModifyEvents.tracks_named TrackInfo.is_signal_track $
        ModifyEvents.text to_hex

to_hex :: String -> String
to_hex text = case Derive.ParseBs.parse_val (ControlTrack.event_val event) of
    Right (TrackLang.VNum (Score.Typed Score.Untyped n))
        | 0 <= n && n <= 1 -> ControlTrack.unparse $
            event { ControlTrack.event_val = ShowVal.show_hex_val n }
    _ -> text
    where event = ControlTrack.parse text

-- * events

events :: TrackId -> ScoreTime -> ScoreTime -> Cmd.CmdL [Event.Event]
events track_id start end = do
    track <- State.get_track track_id
    return $ (Events.ascending
        . Events.in_range start end . Track.track_events) track

-- * strip controls

drop_dups :: Cmd.CmdL ()
drop_dups = ModifyEvents.selection $ ModifyEvents.events $
    return . Seq.drop_dups Event.event_bytestring


-- * signal render

filled :: Cmd.CmdL ()
filled = do
    (block_id, _, track_id, _) <- Selection.get_insert
    PlayUtil.clear_cache block_id
    State.set_render_style Track.Filled track_id

line :: Cmd.CmdL ()
line = do
    (block_id, _, track_id, _) <- Selection.get_insert
    PlayUtil.clear_cache block_id
    State.set_render_style Track.Line track_id

no_render :: Cmd.CmdL ()
no_render = do
    (block_id, _, track_id, _) <- Selection.get_insert
    PlayUtil.clear_cache block_id
    State.set_render_style Track.NoRender track_id
