-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments' FM8 softsynth.
module Local.Instrument.Fm8 where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Perform.Midi.Instrument as Instrument


load :: FilePath -> IO (Maybe MidiInst.Synth)
load _dir = return $ Just $
    MidiInst.with_patches (map MidiInst.with_empty_code patches) $
    Instrument.synth "fm8" "Native Instruments FM8" controls
    where
    controls =
        [ (4, "fm8-control-1"), (11, "fm8-control-2")
        , (16, "morph-x"), (17, "morph-y")
        ]

patches :: [Instrument.Patch]
patches =
    [ Instrument.default_patch pb_range []
    -- TODO this is just because the instrument I use this for has a bit
    -- of decay.  If this sort of thing becomes common maybe I'll need a
    -- per-score way to override instrument parameters.
    , Instrument.set_decay 0.4 $ MidiInst.pressure $
        MidiInst.patch pb_range "pressure" []
    ]
    where
    pb_range = (-96, 96)
