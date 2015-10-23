-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Modartt's amazing Pianoteq softsynth.
module Local.Instrument.Pianoteq where
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Controls as Controls
import qualified Derive.Instrument.Bali as Bali
import qualified Derive.Score as Score

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.NN as NN
import Global


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "pianoteq" "Modartt Pianoteq" pb_range controls)
    { MidiInst.extra_patches = patches }

pb_range :: Instrument.PbRange
pb_range = (-24, 24)

controls :: [(Midi.Control, Score.Control)]
controls =
    [ (67, "soft-pedal")
    , (69, "harmonic-pedal")
    , (66, "sost-pedal")
    , (64, Controls.pedal)
    -- whole bunch more
    ]

patches :: [MidiInst.Patch]
patches =
    [ (patch "pasang" [], Bali.pasang_code)
    , (MidiInst.nn_range (NN.g2, NN.a6) $ patch "yangqin" [], mempty)
    , (patch "harp" harp_controls, mempty)
    ]
    where
    harp_controls =
        [ (67, "gliss")
        , (69, "harmonic")
        , (66, "lute")
        ]

patch :: Instrument.InstrumentName -> [(Midi.Control, Score.Control)]
    -> Instrument.Patch
patch name controls =
    Instrument.patch $ Instrument.instrument name controls pb_range
