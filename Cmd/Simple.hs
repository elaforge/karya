-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Simple Events are supposed to be easy to read, and easy to serialize to
    text and load back again.  Functions here convert them to and from text
    form, stashing converted simple blocks in the clipboard.
-}
module Cmd.Simple where
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.StateConfig as StateConfig
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Cmd.Clip as Clip
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection

import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Types as Midi.Types
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Instrument.InstTypes as InstTypes
import qualified App.Config as Config
import Global
import Types


-- | Dump a score, or part of a score, to paste into a test.
-- (global_transform, allocations, blocks)
type State = (Text, Allocations, [Block])

-- | (id_name, title, tracks, skeleton)
type Block = (Text, Text, [Maybe Track], [Skeleton.Edge])

-- | (id_name, title, events)
type Track = (Text, Text, [Event])

-- | (start, duration, text)
type Event = (Double, Double, Text)

-- | (start, duration, text, initial_nn)
type ScoreEvent = (Double, Double, String, Maybe Pitch.NoteNumber)

-- | (inst, start, duration, initial_nn)
type PerfEvent = (String, Double, Double, Pitch.NoteNumber)

-- | (instrument, (qualified, [(device, chan)]))
--
-- [] chans means it's StateConfig.Dummy.
type Allocations = [(Text, (Text, [(Text, Midi.Channel)]))]

from_score :: ScoreTime -> Double
from_score = ScoreTime.to_double

from_real :: RealTime -> Double
from_real = RealTime.to_seconds

event :: Event.Event -> Event
event e = (from_score s, from_score d, Event.text e)
    where (s, d) = Event.orientation_as_duration e

score_event :: Score.Event -> ScoreEvent
score_event evt =
    ( from_real (Score.event_start evt)
    , from_real (Score.event_duration evt)
    , untxt $ Score.event_text evt
    , Score.initial_nn evt
    )

perf_event :: Midi.Types.Event -> PerfEvent
perf_event evt =
    ( untxt $ Score.instrument_name $ Midi.Types.patch_name $
        Midi.Types.event_patch evt
    , from_real start
    , from_real (Midi.Types.event_duration evt)
    , Pitch.nn (Signal.at start (Midi.Types.event_pitch evt))
    )
    where start = Midi.Types.event_start evt

-- * state

dump_state :: State.M m => m State
dump_state = do
    state <- State.get
    blocks <- mapM dump_block (Map.keys (State.state_blocks state))
    return
        ( State.config#State.global_transform #$ state
        , dump_allocations $ State.config#State.allocations #$ state
        , blocks
        )

load_state :: State.M m => State -> m State.State
load_state (global_transform, allocs, blocks) =
    State.exec_rethrow "convert state" State.empty $ do
        mapM_ make_block blocks
        State.modify $
            (State.config#State.global_transform #= global_transform)
            . (State.config#State.allocations #= allocations allocs)

-- * block

dump_block :: State.M m => BlockId -> m Block
dump_block block_id = do
    block <- State.get_block block_id
    tracks <- mapM dump_tracklike (Block.block_tracklike_ids block)
    tree <- TrackTree.track_tree_of block_id
    return (Id.ident_text block_id, Block.block_title block, tracks,
        to_skel tree)
    where
    to_skel = concatMap go
        where
        go (Tree.Node track subs) =
            [(num track, num (Tree.rootLabel sub)) | sub <- subs]
            ++ to_skel subs
    num = State.track_tracknum

load_block :: Cmd.M m => Block -> m State.State
load_block block = State.exec_rethrow "convert block" State.empty $
    make_block block

load_block_to_clip :: FilePath -> Cmd.CmdT IO ()
load_block_to_clip fn = read_block fn >>= Clip.state_to_clip

read_block :: FilePath -> Cmd.CmdT IO State.State
read_block fn = do
    simple_block <- liftIO (readIO =<< readFile fn :: IO Block)
    load_block simple_block

make_block :: State.M m => Block -> m BlockId
make_block (id_name, title, tracks, skel) = do
    tracks <- mapM load_tracklike tracks
    block_id <- State.create_block (Id.read_id id_name) title tracks
    State.set_skeleton block_id (Skeleton.make skel)
    return block_id

dump_tracklike :: State.M m => Block.TracklikeId -> m (Maybe Track)
dump_tracklike =
    maybe (return Nothing) (fmap Just . dump_track) . Block.track_id_of

load_tracklike :: State.M m => Maybe Track -> m Block.Track
load_tracklike Nothing = return $ Block.track (Block.RId State.no_ruler) 0
load_tracklike (Just track) = load_track track

-- * track

dump_track :: State.M m => TrackId -> m Track
dump_track track_id = do
    track <- State.get_track track_id
    return (simplify_track track_id track)

simplify_track :: TrackId -> Track.Track -> Track
simplify_track track_id track =
    (Id.ident_text track_id, Track.track_title track, map event events)
    where events = Events.ascending (Track.track_events track)

load_track :: State.M m => Track -> m Block.Track
load_track (id_name, title, events) = do
    track_id <- State.create_track (Id.read_id id_name) $
        Track.track title (Events.from_list (map load_event events))
    return $ Block.track (Block.TId track_id State.no_ruler) Config.track_width

load_event :: Event -> Event.Event
load_event (start, dur, text) =
    Event.event (ScoreTime.double start) (ScoreTime.double dur) text

dump_selection :: Cmd.CmdL [(TrackId, [Event])]
dump_selection = map (second (map event)) <$> Selection.events

-- * allocations

dump_allocations :: StateConfig.Allocations -> Allocations
dump_allocations (StateConfig.Allocations allocs) = do
    (inst, alloc) <- Map.toList allocs
    let addrs = case StateConfig.alloc_backend alloc of
            StateConfig.Midi config -> addrs_of config
            StateConfig.Im -> []
            StateConfig.Dummy -> []
    let qualified = InstTypes.show_qualified $ StateConfig.alloc_qualified alloc
    return (Score.instrument_name inst, (qualified, addrs))
    where
    addrs_of config =
        [ (Midi.write_device_text dev, chan)
        | (dev, chan) <- Patch.config_addrs config
        ]

allocations :: Allocations -> StateConfig.Allocations
allocations allocs = StateConfig.Allocations $ Map.fromList $ do
    (inst, (qualified, addrs)) <- allocs
    let backend = case addrs of
            [] -> StateConfig.Dummy
            _ -> StateConfig.Midi $ Patch.config $
                map (first Midi.write_device) addrs
        alloc = StateConfig.allocation (InstTypes.parse_qualified qualified)
            backend
    return (Score.Instrument inst, alloc)


-- * ExactPerfEvent

-- | Like 'PerfEvent', but is meant to recreate a 'Midi.Types.Event' exactly.
type ExactPerfEvent =
    ( Text, RealTime, RealTime, [(Text, [(RealTime, Signal.Y)])]
    , [(RealTime, Signal.Y)], (Signal.Y, Signal.Y), Stack.Stack
    )

dump_exact_perf_event :: Midi.Types.Event -> ExactPerfEvent
dump_exact_perf_event (Midi.Types.Event start dur patch controls pitch svel evel
        stack) =
    ( Score.instrument_name (Midi.Types.patch_name patch)
    , start, dur
    , map (Score.control_name *** Signal.unsignal) (Map.toList controls)
    , Signal.unsignal pitch
    , (svel, evel)
    , stack
    )

load_exact_perf_event :: (InstTypes.Qualified -> Maybe Midi.Types.Patch)
    -> ExactPerfEvent -> Maybe Midi.Types.Event
load_exact_perf_event lookup_patch (inst, start, dur, controls, pitch,
        (svel, evel), stack) = do
    patch <- lookup_patch (InstTypes.parse_qualified inst)
    return $ Midi.Types.Event
        { event_patch = patch
        , event_start = start
        , event_duration = dur
        , event_controls = control_map controls
        , event_pitch = Signal.signal pitch
        , event_start_velocity = svel
        , event_end_velocity = evel
        , event_stack = stack
        }

control_map :: [(Text, [(RealTime, Signal.Y)])] -> Midi.Types.ControlMap
control_map kvs =
    Map.fromList [(Score.unchecked_control k, Signal.signal v) | (k, v) <- kvs]
