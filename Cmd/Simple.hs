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
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Cmd.Clip as Clip
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified App.Config as Config
import Global
import Types


-- | Dump a score, or part of a score, to paste into a test.
-- (global_transform, midi_config, aliases, blocks)
type State = (Text, MidiConfig, Aliases, [Block])
type Aliases = [(Text, Text)]

-- | TODO should it have a ruler?  Otherwise they come in without a ruler...
-- but copy and paste can't copy and paste the ruler.
--
-- (id_name, title, tracks, skeleton)
type Block = (String, Text, [Track], [Skeleton.Edge])

-- | (id_name, title, events)
type Track = (String, Text, [Event])

-- | (start, duration, text)
type Event = (Double, Double, Text)

-- | (start, duration, text, initial_nn)
type ScoreEvent = (Double, Double, String, Maybe Pitch.NoteNumber)

-- | (inst, start, duration, initial_nn)
type PerfEvent = (String, Double, Double, Pitch.NoteNumber)

-- | (instrument, [(device, chan)])
type MidiConfig = [(Text, [(Text, Midi.Channel)])]

from_score :: ScoreTime -> Double
from_score = ScoreTime.to_double

from_real :: RealTime -> Double
from_real = RealTime.to_seconds

event :: Event.Event -> Event
event event = (from_score (Event.start event),
    from_score (Event.duration event), Event.event_text event)

score_event :: Score.Event -> ScoreEvent
score_event evt = (from_real (Score.event_start evt),
    from_real (Score.event_duration evt),
    untxt $ Score.event_text evt, Score.initial_nn evt)

perf_event :: Perform.Event -> PerfEvent
perf_event evt =
    ( untxt $ Instrument.inst_name (Perform.event_instrument evt)
    , from_real start
    , from_real (Perform.event_duration evt)
    , Pitch.nn (Signal.at start (Perform.event_pitch evt))
    )
    where start = Perform.event_start evt

dump_state :: State.M m => m State
dump_state = do
    state <- State.get
    blocks <- mapM dump_block (Map.keys (State.state_blocks state))
    return
        ( State.config#State.global_transform #$ state
        , dump_midi_config $ State.config#State.midi #$ state
        , map (Score.inst_name *** Score.inst_name) . Map.toList $
            State.config#State.aliases #$ state
        , blocks
        )

dump_block :: State.M m => BlockId -> m Block
dump_block block_id = do
    block <- State.get_block block_id
    let track_ids = Block.block_track_ids block
    tracks <- mapM dump_track track_ids
    tree <- TrackTree.track_tree_of block_id
    return (Id.ident_string block_id, Block.block_title block, tracks,
        to_skel tree)
    where
    to_skel = concatMap go
        where
        go (Tree.Node track subs) =
            [(num track, num (Tree.rootLabel sub)) | sub <- subs]
            ++ to_skel subs
    num = State.track_tracknum

dump_track :: State.M m => TrackId -> m Track
dump_track track_id = do
    track <- State.get_track track_id
    return (simplify_track track_id track)

simplify_track :: TrackId -> Track.Track -> Track
simplify_track track_id track =
    (Id.ident_string track_id, Track.track_title track, map event events)
    where events = Events.ascending (Track.track_events track)

dump_selection :: Cmd.CmdL [(TrackId, [Event])]
dump_selection = do
    track_events <- Selection.events
    return [(track_id, map event events)
        | (track_id, _, events) <- track_events]

dump_midi_config :: Instrument.Configs -> MidiConfig
dump_midi_config configs =
    [(Score.inst_name inst, chans_of config)
        | (inst, config) <- Map.toList configs]
    where
    chans_of config = [(Midi.write_device_text dev, chan)
        | ((dev, chan), _) <- Instrument.config_addrs config]


-- * load

convert_state :: State.M m => State -> m State.State
convert_state (global_transform, midi, aliases, blocks) =
    State.exec_rethrow "convert state" State.empty $ do
        mapM_ make_block blocks
        State.modify $
            (State.config#State.global_transform #= global_transform)
            . (State.config#State.midi #= midi_config midi)
            . (State.config#State.aliases #=
                Map.fromList (map (Score.Instrument *** Score.Instrument)
                    aliases))

load_block_to_clip :: FilePath -> Cmd.CmdT IO ()
load_block_to_clip fn = read_block fn >>= Clip.state_to_clip

read_block :: FilePath -> Cmd.CmdT IO State.State
read_block fn = do
    simple_block <- liftIO (readIO =<< readFile fn :: IO Block)
    convert_block simple_block

convert_block :: Cmd.M m => Block -> m State.State
convert_block block = State.exec_rethrow "convert block" State.empty $
    make_block block

make_block :: State.M m => Block -> m BlockId
make_block (id_name, title, tracks, skel) = do
    tracks <- mapM convert_track tracks
    block_id <- State.create_block (Id.read_id (txt id_name)) title tracks
    State.set_skeleton block_id (Skeleton.make skel)
    return block_id

convert_track :: State.M m => Track -> m Block.Track
convert_track (id_name, title, events) = do
    track_id <- State.create_track (Id.read_id (txt id_name)) $
        Track.track title (Events.from_list (map convert_event events))
    return $ Block.track (Block.TId track_id State.no_ruler) Config.track_width

convert_event :: Event -> Event.Event
convert_event (start, dur, text) =
    Event.event (ScoreTime.double start) (ScoreTime.double dur) text

midi_config :: MidiConfig -> Instrument.Configs
midi_config config = Instrument.configs
    [(Score.Instrument inst, map mkaddr chans) | (inst, chans) <- config]
    where mkaddr (dev, chan) = (Midi.write_device dev, chan)
