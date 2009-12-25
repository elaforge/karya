module Derive.DeriveTest where
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Util.Log as Log

import qualified Midi.Midi as Midi
import qualified Instrument.MidiDb as MidiDb

import Ui
import qualified Ui.UiTest as UiTest
import qualified Ui.State as State

import qualified Derive.Derive as Derive
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Schema as Schema
import qualified Derive.Score as Score

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Perform.Transport as Transport
import qualified Perform.Warning as Warning

import qualified Perform.Midi.Control as Midi.Control
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform


scale_id = Twelve.scale_id

pitch_signal :: Pitch.ScaleId -> [PitchSignal.Segment]
    -> PitchSignal.PitchSignal
pitch_signal scale_id =
    PitchSignal.track_signal scale_id PitchSignal.default_srate

signal = Signal.track_signal Signal.default_srate

-- * derive

type Result a = (Either String a, Transport.TempoFunction,
    Transport.InverseTempoFunction, [Log.Msg], Derive.State)

derive_tracks :: [UiTest.TrackSpec] -> Result [Score.Event]
derive_tracks tracks = derive_block ui_state UiTest.default_block_id
    where (_, ui_state) = UiTest.run_mkstate tracks

derive_tracks_tempo :: [UiTest.TrackSpec] -> Result [Score.Event]
derive_tracks_tempo tracks = derive_tracks (("tempo", [(0, 0, "1")]) : tracks)

perform :: Instrument.Config -> [Score.Event]
    -> ([Perform.Event], [Warning.Warning], [Midi.Message], [Warning.Warning])
perform inst_config events = (perf_events, convert_warns, mmsgs, perform_warns)
    where
    (perf_events, convert_warns) = Convert.convert default_lookup events
    (msgs, perform_warns) =
        Perform.perform default_lookup inst_config perf_events
    mmsgs = map Midi.wmsg_msg msgs

derive_block :: State.State -> BlockId -> Result [Score.Event]
derive_block ui_state block_id = derive lookup_deriver ui_state deriver
    where
    lookup_deriver = Schema.lookup_deriver Map.empty ui_state
    deriver = Derive.d_block UiTest.default_block_id

derive :: Derive.LookupDeriver -> State.State
    -> Derive.DeriveT Identity.Identity a
    -> Result a
derive lookup_deriver ui_state deriver =
    case Derive.derive lookup_deriver ui_state False deriver of
        (Left err, b, c, d, e) -> (Left (show err), b, c, d, e)
        (Right a, b, c, d, e) -> (Right a, b, c, d, e)

-- ** extract

e_val :: Result a -> (Either String a, [Log.Msg])
e_val (val, _, _, msgs, _) = (val, msgs)


-- * inst

default_lookup :: MidiDb.LookupMidiInstrument
default_lookup attrs (Score.Instrument inst)
    | inst == "synth/patch" = Just default_perf_inst
    | otherwise = Nothing

default_inst = Score.Instrument "synth/patch"
default_perf_inst = Instrument.instrument "synth" "patch" Nothing
            Midi.Control.empty_map (-2, 2)
default_inst_title = ">synth/patch"

default_inst_config = Instrument.config
    [(default_inst, [(Midi.WriteDevice "out", 0)])] Nothing

{-
default_derive_tracks :: [UiTest.TrackSpec]
    -> ([Score.Event], [Log.Msg], [Perform.Event], [Warning.Warning],
        [Midi.Message], [Warning.Warning])
default_derive_tracks tracks = default_derive ui_state default_inst_config
    where (_, ui_state) = UiTest.run_mkstate tracks

-- | Fake up a stack and track warp, so I can derive without d_block or
-- d_warp.
setup_deriver d = Derive.with_stack_block block_id (Derive.start_new_warp >> d)
-}
