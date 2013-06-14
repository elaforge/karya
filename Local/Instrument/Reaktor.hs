-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments' Reaktor softsynth.
module Local.Instrument.Reaktor where
import Util.Control
import qualified Midi.CC as CC
import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make
    (MidiInst.softsynth "reak" "Native Instruments Reaktor" pb_range [])
    { MidiInst.extra_patches = MidiInst.with_empty_code patches }

pb_range = (-96, 96)

patches :: [Instrument.Patch]
patches =
    -- My own patches.
    [ MidiInst.pressure $ MidiInst.patch pb_range "fm1" [(4, "depth")]
    , Instrument.text #= "Tunable comb filter that processes an audio signal." $
        MidiInst.patch pb_range "comb" [(1, "mix"), (4, "fbk")]

    -- Commercial patches.

    , MidiInst.patch pb_range "spark"     -- Chords Sister
        [ (4, "mc1"), (11, "mc2"), (1, "mc3")
        , (24, "cutoff"), (25, "q")
        ]

    -- Downloaded patches.

    , MidiInst.patch pb_range "shark"
        [ (4, "cutoff1"), (3, "q") -- 1st filter
        , (10, "color")
        ]

    , Instrument.text #= "Herald brass physical model." $
        -- Downloaded from NI, Herald_Brass_V2.ens.
        -- Modifications: disconnect the PM port and replace with pitch bend of
        -- 96.  Assign controls to knobs.
        -- Flutter and vib are just macros for air and emb controls, but seem
        -- useful.
        MidiInst.pressure $ MidiInst.patch pb_range "herald"
            [ (CC.mod, "vib")
            , (CC.vib_speed, "vib-speed")
            , (CC.cc14, "atk") -- tongue attack
            , (CC.cc15, "buzz") -- tongue buzz
            , (CC.cc16, "buzz-len") -- tongue buzz length
            , (CC.cc17, "emb") -- lips embouchure
            , (CC.cc18, "stiff") -- lips stiffness
            -- , (CC.cc19, "noise") -- lips noise, not implemented
            , (CC.cc20, "finger") -- bore finger time

            , (CC.cc21, "flut") -- flutter tongue
            , (CC.cc22, "flut-speed") -- flutter tongue speed
            ]

    , Instrument.text #= "Serenade bowed string physical model." $
        -- Downloaded from NI, Serenade.ens.
        -- Modifications: Remove gesture and replace with a direct mapping to
        -- cc2.  Add pitch bend to pitch.  Assign controls to knobs.
        --
        -- I map breath to only one bowing direction, since deciding on which
        -- direction the bow is going all the time seems like a pain, and
        -- probably has minimal affect on the sound.  If dropping to
        -- 0 momentarily sounds like a direction change then that's good
        -- enough.
        MidiInst.pressure $ MidiInst.patch pb_range "serenade"
            [ (CC.mod, "vib")
            , (CC.vib_speed, "vib-speed")
            , (CC.cc14, "bow-speed")
            , (CC.cc15, "bow-force")
            , (CC.cc16, "bow-pos")
            , (CC.cc17, "string-jitter")
            , (CC.cc18, "string-buzz")
            , (CC.cc20, "pizz") -- 0 == arco, 127 = pizz
            , (CC.cc21, "pizz-tone")
            , (CC.cc22, "pizz-time")
            , (CC.cc23, "pizz-level")
            ]
    ]
