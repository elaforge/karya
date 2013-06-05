-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments' FM8 softsynth.
module Local.Instrument.Fm8 where
import qualified Data.Text as Text

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "fm8" "Native Instruments FM8" pb_range controls)
        { MidiInst.extra_patches = MidiInst.with_empty_code patches }

pb_range = (-96, 96)

controls :: [(Midi.Control, Text.Text)]
controls =
    [ (4, "fm8-control-1"), (11, "fm8-control-2")
    , (16, "morph-x"), (17, "morph-y")
    ]

patches :: [Instrument.Patch]
patches =
    [ MidiInst.pressure $ MidiInst.patch pb_range "pressure" []
    ]
