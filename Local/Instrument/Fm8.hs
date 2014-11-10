-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments' FM8 softsynth.
module Local.Instrument.Fm8 where
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "fm8" "Native Instruments FM8" pb_range controls)
        { MidiInst.extra_patches = map MidiInst.with_empty_code patches }

pb_range = (-96, 96)

controls :: [(Midi.Control, Score.Control)]
controls =
    [ (4, "fm8-control-1"), (11, "fm8-control-2")
    , (16, "morph-x"), (17, "morph-y")
    ]

patches :: [Instrument.Patch]
patches =
    -- TODO this is just because the instrument I use this for has a bit
    -- of decay.  If this sort of thing becomes common maybe I'll need a
    -- per-score way to override instrument parameters.
    [ Instrument.set_decay 0.4 $ MidiInst.pressure $
        MidiInst.patch pb_range "pressure" []
    ]
