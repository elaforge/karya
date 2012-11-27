-- | Spicy guitar, free at http://www.spicyguitar.com/
module Local.Instrument.Spicy where
import Util.Control
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Derive.Attrs as Attrs
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "spicy" pb_range controls)
        { MidiInst.modify_wildcard =
            (Instrument.instrument_#Instrument.hold_keyswitch #= True)
            . Instrument.set_keyswitches keyswitches
        }

pb_range = (-3, 3)

-- WARNING: changing these while playing tends to crash the vst.
controls :: [(Midi.Control, String)]
controls =
    [ (20, "position") -- 0 for bridge, 1 for middle
    , (21, "finger") -- 0 for finger plucking, 1 for pick
    , (22, "inharmonicity")
    , (23, "twang")
    , (24, "color")
    , (25, "impedance")
    , (26, "vibrato") -- speed of vibrato
    , (27, "mute") -- amount of palm mute effect
    , (28, "harmonic")
    ]

keyswitches :: [(Score.Attributes, Midi.Key)]
keyswitches =
    [ (Attrs.legato, Key.b2)
    , (Attrs.mute, Key.c3)
    , (Attrs.harmonic, Key.cs3)
    ]
