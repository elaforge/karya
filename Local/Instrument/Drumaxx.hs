-- | Image-Line's Drumaxx softsynth.
module Local.Instrument.Drumaxx where
import Util.Control
import Midi.Key
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.Drums as Drums
import Derive.Attrs
import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "dmx" (-24, 24) [])
        { MidiInst.modify_patch =
            Instrument.set_flag Instrument.Triggered . Instrument.set_keymap
                [(Drums.note_attrs n, key) | (n, key) <- notes]
        , MidiInst.code = MidiInst.empty_code
            { MidiInst.note_calls = [Drums.make_calls (map fst notes)]
            , MidiInst.cmds = [Drums.make_cmd notes]
            }
        }

-- | The octave numbers on the drummax are one greater than the standard usage.
-- This is for \"Acoustic 2 FG\".  I'll have to come up with a standard mapping
-- later.
notes :: [(Drums.Note, Midi.Key)]
notes =
    [ (Drums.c_bd, c2)
    , (Drums.c_sn, d2)
    , (Drums.Note "sn'" (snare <> v1) 'X', e2)
    , (Drums.c_ltom, g2)
    , (Drums.c_mtom, b2)
    , (Drums.c_htom, c3)
    , (Drums.c_hh, fs2)
    , (Drums.c_ride, ds3)
    , (Drums.c_crash, cs3)
    ]
