-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Modartt's amazing Pianoteq softsynth.
module Local.Instrument.Pianoteq where
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Controls as Controls
import qualified Derive.Instrument.Bali as Bali
import qualified Derive.Score as Score

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.NN as NN
import Global


load :: FilePath -> IO (Maybe MidiInst.Synth)
load _dir = return $ Just $
    MidiInst.with_patches patches $
    (Instrument.synth "pianoteq" "Modartt Pianoteq" [])
        { Instrument.synth_supports_realtime_tuning = True }

pb_range :: Instrument.PbRange
pb_range = (-24, 24)

patches :: [MidiInst.Patch]
patches =
    [ MidiInst.with_empty_code $ Instrument.default_patch pb_range
        [ (67, "soft-pedal")
        , (69, "harmonic-pedal")
        , (66, "sost-pedal")
        , (64, Controls.pedal)
        -- whole bunch more
        ]
    , (patch "pasang" [], Bali.pasang_code)
    , (MidiInst.nn_range (NN.g2, NN.a6) $ patch "yangqin" [], mempty)
    , harp
    ]

-- | TODO harp rings by default until explicitly damped.  How can I best
-- represent that?
harp :: MidiInst.Patch
harp = MidiInst.with_code code $ patch "harp"
        [ (67, "gliss")
        , (69, harmonic)
        , (66, lute)
        ]
    where
    code :: MidiInst.Code
    code = MidiInst.note_calls
        [ MidiInst.both "o" $ Make.control_note Module.instrument "o" harmonic 1
        , MidiInst.both "m" $ Make.control_note Module.instrument "m" lute 1
        ]
    -- TODO add diatonic gliss call, like zheng
    harmonic = "harmonic"
    lute = "lute"

patch :: Instrument.InstrumentName -> [(Midi.Control, Score.Control)]
    -> Instrument.Patch
patch = MidiInst.patch pb_range
