-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments' Reaktor softsynth.
module Local.Instrument.Reaktor where
import Util.Control
import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make
    (MidiInst.softsynth "reak" "Native Instruments Reaktor" pb_range [])
    { MidiInst.extra_patches = MidiInst.with_empty_code patches }

pb_range = (-36, 36)

patches :: [Instrument.Patch]
patches =
    [ MidiInst.pressure $ MidiInst.patch pb_range "fm1" [(4, "depth")]
    , Instrument.text #= "Tunable comb filter that processes an audio signal." $
        MidiInst.patch pb_range "comb" [(1, "mix"), (4, "fbk")]
    ]
