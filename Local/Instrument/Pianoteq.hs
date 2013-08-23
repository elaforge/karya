-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Modartt's amazing Pianoteq softsynth.
module Local.Instrument.Pianoteq where
import qualified Midi.Midi as Midi
import qualified Derive.Controls as Controls
import qualified Derive.Score as Score
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    MidiInst.softsynth "ptq" "Modartt Pianoteq" (-24, 24) controls

controls :: [(Midi.Control, Score.Control)]
controls =
    [ (64, Controls.pedal)
    , (67, "soft-pedal")
    , (66, "sost-pedal")
    , (69, "harmonic-pedal")
    -- whole bunch more
    ]
