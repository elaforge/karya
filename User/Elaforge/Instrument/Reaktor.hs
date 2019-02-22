-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments' Reaktor softsynth.
module User.Elaforge.Instrument.Reaktor where
import qualified Data.Set as Set

import qualified Util.Doc as Doc
import qualified Midi.CC as CC
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Controls as Controls
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Score as Score

import qualified Perform.Midi.Patch as Patch
import qualified Instrument.InstTypes as InstTypes
import Global


synth :: MidiInst.Synth
synth = MidiInst.synth "reaktor" "Native Instruments Reaktor" patches

resonant_filter :: MidiInst.Code
resonant_filter = MidiInst.null_call $
    DUtil.double_pitch "res" Nothing "res"
        (Just (Set.fromList ["mix", "q", "lp-hp", "2-4-pole"]))

patch :: InstTypes.Name -> [(Midi.Control, Score.Control)] -> MidiInst.Patch
patch = MidiInst.named_patch (-96, 96)

patches :: [MidiInst.Patch]
patches =
    -- My own patches.
    [ MidiInst.pressure $ patch "fm1" [(4, "depth")]
    , MidiInst.doc #= "Tunable comb filter that processes an audio signal." $
        patch "comb" [(1, "mix"), (4, "fbk")]
    , MidiInst.doc #= "Tunable filter that processes an audio signal." $
        patch "filter"
            [ (1, "mix")
            , (CC.cc14, "q")
            , (CC.cc15, "lp-hp")
            , (CC.cc16, "2-4-pole")
            ]

    -- Factory patches.
    , patch "lazerbass"
        -- In 'parameter', replace bend input of 'Basic Pitch prepare' with 96,
        -- replace 'M.tr' input of 'Global P/G' with 0.
        -- Rebind ccs for c1 and c2.
        [ (CC.cc14, Controls.mc1), (CC.cc15, Controls.mc2)
        ]
    , patch "steam"
        -- Steampipe2, set pitch bend range to 96.
        []

    -- Commercial patches.

    , patch "spark"
        -- Modifications:
        -- Bind cc14 to red "filter cutoff" knob, smoothing from 50 to 10.
        -- Bind cc15 to red "filter reso" knob.
        -- Bind cc7 to "output" knob.
        -- Hardcode pitchbend range to 96 by replacing control with a constant.
        [ (4, Controls.mc1), (11, Controls.mc2), (1, Controls.mc3)
        , (CC.cc14, Controls.lpf)
        , (CC.cc15, Controls.q)
        ]
    , patch "prism"
        [ (1, Controls.mc1)
        , (11, Controls.mc2)
        ]

    -- Downloaded patches.

    , patch "shark"
        -- Downloaded from NI, Shark.ens.
        -- Modifications: pitchbend to 96, signal smoothers from 100ms to 10ms.
        [ (4, Controls.lpf), (3, Controls.q) -- 1st filter
        , (10, "color")
        ]

    , MidiInst.doc #= "Herald brass physical model." $
        -- Downloaded from NI, Herald_Brass_V2.ens.
        -- Modifications: disconnect the PM port and replace with pitch bend of
        -- 96.  Assign controls to knobs.
        -- Flutter and vib are just macros for air and emb controls, but seem
        -- useful.
        MidiInst.pressure $ patch "herald"
            [ (CC.mod, Controls.vib)
            , (CC.vib_speed, Controls.vib_speed)
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

    , MidiInst.doc #= "Serenade bowed string physical model." $
        -- Downloaded from NI, Serenade.ens.
        -- Modifications: Remove gesture and replace with a direct mapping to
        -- cc2.  Add pitch bend to pitch.  Assign controls to knobs.
        --
        -- It's important to put the pitch bend in "Bowed String", after the
        -- tuner.
        --
        -- I map breath to only one bowing direction, since deciding on which
        -- direction the bow is going all the time seems like a pain, and
        -- probably has minimal affect on the sound.  If dropping to
        -- 0 momentarily sounds like a direction change then that's good
        -- enough.
        MidiInst.patch # Patch.attribute_map #=
            Patch.cc_keyswitches [(CC.cc20, [(Attrs.pizz, 127), (mempty, 0)])] $
        MidiInst.pressure $ MidiInst.named_patch (-24, 24) "serenade"
            [ (CC.mod, Controls.vib)
            , (CC.vib_speed, Controls.vib_speed)
            , (CC.cc14, "bow-speed")
            , (CC.cc15, "bow-force")
            , (CC.cc16, "bow-pos")
            , (CC.cc17, "string-jitter")
            , (CC.cc18, "string-buzz")
            , (CC.cc21, "pizz-tone")
            , (CC.cc22, "pizz-time")
            , (CC.cc23, "pizz-level")
            ]
    ] ++ silverwood_patches

{- | Downloaded from NI, SilverwoodV3.2.ens.
    Modifications: add AR envelope to kbd gate multiplied with breath, so key
    off also causes the note to stop.  Vibrato all snap isolated, and all the
    "Mod" controls turned off.  Rate, Level1, and Level2 directly mapped to
    controls, defaults all 0.  Growl all snap isolated, destination pressure,
    level 0 and mapped.

    Replace Pitch Wheel in MIDI Mod Sources with constant 0, and replace
    transpose with PitchBend in Pitch.

    Each model has different controls, and the controls that aren't mapped to
    air pressure or note number are simplified to be directly controllable from
    MIDI.  That is, I delete everything except the \"Amount\" knob, and tune its
    range to be what sounds useful for that model.

    - sax (oboe, english horn, bassoon, harmonica, accordion, uillean pipes,
    scots pipes): exc. pt (0--?), effic. (-1--?)

    - clarinet: register (0--0.5)

    - flute (piccolo, irish whistle, shakuhachi): embouch (-45--45)

    - recorder (pan pipes): none

    - brass (trumpet, cornet, flugelhorn, french horn, trombone, euphonium,
    tuba): embouch (-45--45)

    Nothing for organ model yet, I don't really care much about organs.
-}
silverwood_patches :: [MidiInst.Patch]
silverwood_patches =
    [ mk "clarinet" [(CC.cc16, "register")]
    , mk "flute" [(CC.cc17, "embouch")]
    , mk "brass" [(CC.cc17, "embouch")]
    , mk "sax" [(CC.cc18, "exc"), (CC.cc19, "effic")]
    , mk "recorder" []
    ]
    where
    mk name controls =
        MidiInst.doc #=
            ("Silverwood woodwind physical model: " <> Doc.Doc name) $
        MidiInst.pressure $ patch ("sw-" <> name) $
            [ (CC.mod, Controls.vib)
            , (CC.vib_speed, Controls.vib_speed)
            , (CC.cc14, "trem") -- brightness "vibrato", destination 1
            , (CC.cc15, "growl")
            ] ++ controls
