module Perform.Midi.Perform_profile where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import Util.Test

import Ui

import qualified Midi.Midi as Midi

import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Instrument as Instrument

import qualified Instrument.MidiDb as MidiDb

import qualified Perform.Signal as Signal


profile_perform = do
    let f evts = (length msgs, logs)
            where
            (msgs, logs, _) = Perform.perform Perform.initial_state
                test_lookup inst_config evts
    let len = 100000
    let sig = force $ Signal.signal (zip [0, 0.25 .. len] (cycle vals))
        vals = map (/10) ([0..10] ++ [10, 9 .. 1])
    let (msgs, logs) = f
            [(mkevent 0 len [(Control.Control "cc1", sig)] Signal.empty)]
    -- Forcing the msgs first leads to a big space drag.  I'm not sure why...
    -- shouldn't forcing the logs result in a much larger unevaluated msgs?
    pprint logs
    pprint msgs
    -- pprint $ f [(mkevent 0 len [(Control.Control "cc1", sig)] Signal.empty)]

profile_perform_control = do
    let len = 300000
    let sig = force $ Signal.signal (zip [0, 0.25 .. len] (cycle vals))
        vals = map (/10) ([0..10] ++ [10, 9 .. 1])
    let (msg, warns) = Perform.perform_control Control.empty_map 0 0
            (Control.Control Control.c_mod, sig)
    pprint warns
    pprint (length msg)


force val = DeepSeq.deepseq val val


mkevent :: RealTime -> RealTime -> [(Control.Control, Signal.Control)]
    -> Signal.NoteNumber -> Perform.Event
mkevent start dur controls pitch_sig =
    Perform.Event inst1 (Timestamp.from_real_time start)
        (Timestamp.from_real_time dur) (Map.fromList controls) pitch_sig
        Stack.empty


test_lookup :: MidiDb.LookupMidiInstrument
test_lookup _ (Score.Instrument inst)
    | inst == "inst1" = Just $ inst1
    | otherwise = Nothing

inst1 = mkinst "inst1"
mkinst name = Instrument.instrument "synth" name Nothing Control.empty_map
    (-1, 1)

inst_config = Instrument.config
    [ (Score.Instrument "inst1", [(dev, 0), (dev, 1)]) ]
    where dev = Midi.WriteDevice "dev1"
