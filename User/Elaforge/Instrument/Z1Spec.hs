-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- In ghc 8 change to:
-- {-# OPTIONS_GHC -Wno-warn-unused-top-binds #-}

-- | Utilities to encode and generate z1 sysex dumps.
--
-- TODO
-- Basic parsing and modification works, but some fields, like effects, are not
-- parsed.
module User.Elaforge.Instrument.Z1Spec (
    decode, encode, specBytes
    , programDumpHeader, currentProgramDumpHeader
    , patchSpec, multisetSpec
) where
import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import qualified Midi.Midi as Midi
import qualified Instrument.Sysex as Sysex
import Instrument.Sysex
       (Specs, Spec(..), bits, rangedBits, unsigned, boolBit, enum, enumBits,
        ranged, signed)
import Global

-- TODO
-- * finish the effects
-- - put them in patch_spec and multiset_spec
-- - finish osc types

config :: Sysex.Config
config = Sysex.config8bit

encode :: Specs -> Sysex.RMap -> Either String ByteString
encode = Sysex.encode config

decode :: Specs -> ByteString -> Either String (Sysex.RMap, ByteString)
decode = Sysex.decode config

specBytes :: Specs -> Int
specBytes = Sysex.specBytes config

assertValid :: String -> Int -> Specs -> Specs
assertValid = Sysex.assertValid config

-- | I think 0x30 should be 0x3g where g is the global channel, but I use 0
-- so this works.
z1Header :: ByteString
z1Header = B.pack [0xf0, Midi.korgCode, 0x30, 0x46]

programDumpHeader :: Specs
programDumpHeader =
    [ ("", Constant z1Header)
    , ("", Constant (B.singleton 0x4c))
    , ("", Bits
        [ ("bank", enumBits 4 ["a", "b"])
        , ("unit", enumBits 4 ["program", "bank", "", "all"])
        ])
    , ("program number", unsigned 127)
    , ("", Constant (B.singleton 0))
    ]

currentProgramDumpHeader :: Specs
currentProgramDumpHeader =
    [ ("", Constant z1Header)
    , ("", Constant $ B.pack [0x40, 0x01])
    ]

-- * program / patch

patchSpec :: Specs
patchSpec = Sysex.assertValid config "patch_spec" 576
    [ ("name", Str 16)
    , ("category", enum categories)
    , ("user group", unsigned 15)
    , ("", Bits
        [ ("hold", bits 1)
        , ("key priority", enumBits 2 ["last", "low", "high"])
        , ("voice assign mode", enumBits 5
            ["mono multi", "mono single", "poly"])
        ])
    , ("retrigger controllor", enum modSourceList2)
    , ("retrigger control threshold", ranged 1 127)
    , ("", Bits
        [ ("unison type", enumBits 2 ["off", "2", "3", "6"])
        , ("unison sw", boolBit)
        , ("unison mode", enumBits 5 ["fixed", "dynamic"])
        ])
    , ("unison detune", unsigned 99)
    -- scale
    , ("", Bits
        [ ("scale key", enumBits 4 scaleKeys)
        , ("scale type", enumBits 4 scaleTypes)
        ])
    , ("random pitch intensity", unsigned 99)
    , ("eg", List 4 egSpec)
    , ("lfo", List 4 lfoSpec)
    -- osc common
    , pitchBend
    , ("common pitch mod source", enum modSourceList1)
    , modIntensity "common pitch"
    , ("portamento", SubSpec
        [ ("", Bits
            [ ("sw", boolBit)
            , ("mode", enumBits 7 ["normal", "fingered"])
            ])
        , ("time", unsigned 99)
        , ("mod source", enum modSourceList1)
        , modIntensity ""
        ])
    , ("osc", List 2 oscSpec)
    , ("sub osc", SubSpec subOscSpec)
    , ("noise generator filter", SubSpec noiseGeneratorSpec)
    , ("mixer", SubSpec mixerSpec)
    , ("filter common", SubSpec filterCommonSpec)
    , ("filter", List 2 filterSpec)
    , ("amp", SubSpec ampSpec)
    , ("output", SubSpec outputSpec)
    , ("effect", SubSpec effectSpec)
    , ("controller", SubSpec controllerSpec)
    , ("link arpeggio", SubSpec (linkArpeggioSpec 7))
    , ("pe knob", List 5 peKnobSpec)
    ]

pitchBend :: (Sysex.Name, Spec)
pitchBend = ("pitch bend", SubSpec
    [ ("intensity +", ranged (-60) 24)
    , ("intensity -", ranged (-60) 24)
    , ("", Bits $
        let vals = ["0", "1/8", "1/4", "1/2"] ++ map showt [1..12] in
        [ ("step +", enumBits 4 vals)
        , ("step -", enumBits 4 vals)
        ])
    ])

egSpec :: Specs
egSpec =
    [ level "start"
    , time "attack"
    , level "attack"
    , time "decay"
    , level "break"
    , time "slope"
    , level "sustain"
    , time "release"
    , level "release"
    , modSource1 "eg level"
    , modIntensity "eg level"
    , ("eg level velocity control", signed 99)
    , modSource1 "eg time"
    , modIntensity "eg time"
    , modSource1 "eg node time"
    , modIntensity "attack time", modIntensity "decay time"
    , modIntensity "slope time", modIntensity "release time"
    ]
    where
    level name = (name <+> "level", signed 99)
    time name = (name <+> "time", unsigned 99)

lfoSpec :: Specs
lfoSpec =
    [ ("", Bits
        [ ("wave form", enumBits 6 lfoWaveforms)
        , ("key sync sw", enumBits 2 ["off", "timbre", "voice"])
        ])
    , ("frequency", unsigned 199)
    , modSource1 "frequency 1"
    , modIntensity "frequency 1"
    , modSource1 "frequency 2"
    , modIntensity "frequency 2"
    , ("fade in", unsigned 99)
    , modSource1 "amplitude"
    , modIntensity "amplitude"
    , ("offset", signed 50)
    , ("", Bits
        [ ("midi sync time", rangedBits 4 (0, 15))
        , ("midi sync base", rangedBits 3 (0, 7))
        , ("midi sync", boolBit)
        ])
    ]

lfoWaveforms :: [Sysex.EnumName]
lfoWaveforms =
    [ "triangle 0", "triangle 90", "tri random", "sine"
    , "saw up 0", "saw up 180", "saw down 0", "saw down 180"
    , "square", "random s/h", "random vector"
    , "step tri 4", "step tri 6", "step saw 4", "step saw 6"
    , "exp tri", "exp saw up", "exp saw down"
    ]

-- | The Z1 has a built-in set of categories. Map the category index to the
-- name.
categories :: [Sysex.EnumName]
categories =
    [ "Synth-Hard", "Synth-Soft", "Synth-Lead", "Synth-Motion", "Synth-Bass"
    , "E.Piano", "Organ", "Keyboard", "Bell", "Strings", "Band/Choir"
    , "Brass", "Reed/Wind", "Guitar/Plucked", "Bass", "Percussive"
    , "Argpeggio", "SFX/Other"
    ]

scaleKeys :: [Sysex.EnumName]
scaleKeys =
    [ "c", "cs", "d", "ds", "e", "f", "g", "gs", "a", "as", "b"]

scaleTypes :: [Sysex.EnumName]
scaleTypes =
    [ "equal temperament", "pure major temperament", "pure minor temperament"
    , "arabic temperament", "pythagorean", "werckmeister", "kirnberger"
    , "slendro", "pelog", "user scale 1", "user scale 2", "user scale 3"
    ]

-- * osc

oscSpec :: Specs
oscSpec =
    [ ("type", enum oscTypes)
    , ("octave", octave)
    , ("semi tone", signed 12)
    , ("fine tune", signed 50)
    , ("frequency offset", signed 100)
    -- pitch slope
    , ("center key", unsigned 127)
    , ("lower slope", ranged (-50) 100)
    , ("higher slope", ranged (-50) 100)
    -- pitch modulation
    , ("mod1 source", enum modSourceList1)
    , intensity "mod1"
    , ("mod1 intensity controller", enum modSourceList1)
    , ("mod1 intensity controller intensity", signed 99)
    , ("mod2 source", enum modSourceList1)
    , ("mod2 intensity", signed 99)
    , ("setting", Unparsed 38) -- union of osc params
    ]

oscTypes :: [Sysex.EnumName]
oscTypes =
    [ "standard", "comb", "vpm", "resonance", "ring-mod", "cross-mod", "sync"
    , "organ", "electric-piano", "brass", "reed", "plucked", "bowed"
    ]

subOscSpec :: Specs
subOscSpec =
    [ ("octave", octave)
    , ("semi tone", signed 12)
    , ("fine tune", signed 50)
    , ("frequency offset", signed 100)
    -- pitch slope
    , ("center key", unsigned 127)
    , ("lower slope", ranged (-50) 100)
    , ("higher slope", ranged (-50) 100)
    -- pitch modulation
    , ("mod1 source", enum modSourceList1)
    , ("mod1 intensity", signed 99)
    , ("mod1 intensity controller", enum modSourceList1)
    , ("mod1 intensity controller intensity", signed 99)
    , ("mod2 source", enum modSourceList1)
    , ("mod2 intensity", signed 99)
    , ("wave form", enum ["saw", "square", "triangle", "sine"])
    ]

octave :: Spec
octave = enum ["-2", "-1", "0", "1"] -- 32', 16', 8', 4'

noiseGeneratorSpec :: Specs
noiseGeneratorSpec =
    [ ("type", enum ["thru", "lpf", "hpf", "bpf"])
    , ("input trim", unsigned 99)
    , ("cutoff", unsigned 99)
    , modSource1 "cutoff mod1"
    , ("cutoff mod1 intensity", signed 99)
    , modSource1 "cutoff mod2"
    , ("cutoff mod2 intensity", signed 99)
    , ("resonance", unsigned 99)
    ]

-- ** osc types

standardOsc :: Specs
standardOsc =
    [ ("wave", enum ["saw", "pulse"])
    , ("wave edge", unsigned 99)
    , ("wave level", unsigned 99)
    , ("triangle level", unsigned 99)
    , ("sine level", unsigned 99)
    , ("triangle phase shift", signed 99)
    , ("wave form", SubSpec
        [ ("wave form", signed 99)
        , ("mod lfo",
            -- TODO For some reason, the range is 6--9, take a look at z1
            enum ["", "", "", "", "", "", "lfo1", "lfo2", "lfo3", "lfo4"])
        , modIntensity "lfo"
        , modSource1 ""
        , modIntensity ""
        ])
    , ("wave shape", SubSpec
        [ ("input level", unsigned 99)
        , modSource1 "input level"
        , modIntensity "input level"
        , ("offset", signed 99)
        , ("table", enum ["clip", "reso"])
        , ("wave shape", unsigned 99)
        , modSource1 ""
        , modIntensity ""
        , ("balance", unsigned 99)
        , modSource1 "balance"
        , modIntensity "balance"
        ])
    ]

combFilterOsc :: Specs
combFilterOsc =
    [ ("input select", enum ["sc1+noise", "subosc+noise", "filter1+noise",
        "filter2+noise", "pulse noise", "impulse"])
    , ("input wave level", unsigned 99)
    , ("noise level", unsigned 99)
    , ("width", unsigned 99)
    , modSource1 "input level"
    , modIntensity "input level"
    , ("comb filter feedback", unsigned 99)
    , modSource1 "feedback 1"
    , modIntensity "feedback 1"
    , modSource1 "feedback 2"
    , modIntensity "feedback 2"
    , ("high damp", unsigned 99)
    , modSource1 "high damp"
    , modIntensity "high damp"
    ]

-- * mixer

mixerSpec :: Specs
mixerSpec =
    [ channel "osc1 out1", channel "osc1 out2"
    , channel "osc2 out1", channel "osc2 out2"
    , channel "sub osc out1", channel "sub osc out2"
    , channel "noise out1", channel "noise out2"
    , channel "feedback out1", channel "feedback out2"
    , ("", Bits
        [ ("osc1 mixer output sw", boolBit)
        , ("osc2 mixer output sw", boolBit)
        , ("sub mixer output sw", boolBit)
        , ("noise mixer output sw", boolBit)
        , Sysex.unparsedBits 4
        ])
    ]
    where
    channel name =
        (name, SubSpec
            [ ("level", unsigned 99)
            , modSource1 "level"
            , modIntensity "level"
            ])

-- * filter

filterCommonSpec :: Specs
filterCommonSpec =
    [ ("", Bits
        [ ("filter routing", enumBits 2 ["serial1", "serial2", "parallel"])
        , ("filter2 link switch", enumBits 2 ["off", "on"])
        , ("filter eg knob target", enumBits 5
            ["", "filter1", "filter2", "both"])
        ])
    ]

filterSpec :: Specs
filterSpec =
    [ ("filter type", enum ["", "lpf", "hpf", "bpf", "brf", "2bpf"])
    , ("a", SubSpec $ filterCommon ++
        [ ("cutoff frequency", SubSpec
            [ ("mod eg", egs)
            , modIntensity "eg"
            , modSource1 "1", modIntensity "1"
            , modSource1 "2", modIntensity "2"
            ])
        -- resonance
        , ("resonance", unsigned 99)
        , modSource1 "resonance"
        , modIntensity "resonance"
        ])
    , ("b", SubSpec $ filterCommon ++
        [ ("cutoff frequency", SubSpec
            [ intensity "mod eg"
            , intensity "mod1"
            , intensity "mod2"
            ])
        , ("resonance", unsigned 99)
        , modIntensity "resonance mod"
        ])
    ]
    where
    filterCommon =
        [ ("input trim", unsigned 99)
        , ("cutoff frequency", unsigned 99)
        -- cutoff keyboard track
        , ("low key", midiKey), ("high key", midiKey)
        , intensity "lower", intensity "higher"
        ]

egs :: Spec
egs = enum ["", "eg1", "eg2", "eg3", "eg4", "amp eg"]
    -- TODO 0 is undocumented but I assume off?  Or invalid?

-- * other

ampSpec :: Specs
ampSpec = assertValid "amp" 37
    [ ("amp", List 2
        [ ("amplitude", unsigned 99)
        , ("low key", midiKey)
        , ("high key", midiKey)
        , intensity "lower"
        , intensity "higher"
        , ("mod eg", egs)
        , ("", Unparsed 1)
        , modSource1 ""
        , modIntensity ""
        ])
    , ("eg", SubSpec
        [ ("", Unparsed 1)
        , time "attack"
        , level "attack"
        , time "decay"
        , level "break"
        , time "slope"
        , level "sustain"
        , time "release"
        , ("", Unparsed 1)
        , modSource1 "eg level"
        , modIntensity "eg level"
        , ("eg velocity control", signed 99)
        , modSource1 "eg time"
        , modIntensity "eg time"
        , modSource1 "eg node time"
        , modIntensity "attack time"
        , modIntensity "decay time"
        , modIntensity "slope time"
        , modIntensity "release time"
        ])
    ]
    where
    level name = (name <+> "level", signed 99)
    time name = (name <+> "time", unsigned 99)

outputSpec :: Specs
outputSpec = assertValid "output" 4
    [ ("panpot", unsigned 127)
    , modSource1 "panpot"
    , modIntensity "panpot"
    , ("output", unsigned 127)
    ]

effectSpec :: Specs
effectSpec = assertValid "effect" 72
    [ ("effect send", unsigned 100)
    , modSource1 "effect send"
    , modIntensity "effect send"
    , ("effect1 select", enum effectType1)
    , ("effect1 setting", Unparsed 22)
    , ("effect2 select", enum effectType1)
    , ("effect2 setting", Unparsed 22)
    , ("master effect select", enum masterEffectType)
    , ("master effect setting", Unparsed 18)
    , ("eq", SubSpec
        [ ("low freq", freq)
        , ("low gain", gain)
        , ("high freq", freq)
        , ("high gain", gain)
        ])
    ]

controllerSpec :: Specs
controllerSpec = assertValid "controller" 4
    [ ("", Bits
        [ ("sw1 function", enumBits 7 sw1Function)
        , ("sw1 mode", latch)
        ])
    , ("", Bits
        [ ("sw2 function", enumBits 7 sw2Function)
        , ("sw2 mode", latch)
        ])
    , ("", Bits
        [ ("foot sw function", enumBits 7 footSwFunction)
        , ("foot sw mode", latch)
        ])
    , ("", Bits
        [ ("pedal function", enumBits 3 pedalFunction)
        , ("pad hold", enumBits 5 ["off", "on"])
        ])
    ]
    where
    latch = enumBits 1 ["latch", "unlatch"]
    sw1Function =
        [ "mod sw 1", "master fx", "fx1", "fx2", "octave up", "octave down"
        , "mono sw", "unison"
        ]
    sw2Function =
        [ "mod sw 2", "master fx", "fx1", "fx2", "octave up", "octave down"
        , "mono sw", "unison"
        ]
    footSwFunction =
        [ "foot sw", "midi damper", "midi portamento", "midi sostenuto"
        , "master fx", "fx1", "fx2", "octave up", "octave down", "mono sw"
        , "unison", "arpeggio", "pad hold"
        ]
    pedalFunction =
        [ "pedal", "breath control", "portamento time", "volume", "panpot"
        , "expression"
        ]

linkArpeggioSpec :: Int -> Specs
linkArpeggioSpec reserved = assertValid "link arpeggio" (6 + reserved)
    [ ("unparsed", Unparsed 6) -- TODO
    , ("", Unparsed reserved)
    ]

peKnobSpec :: Specs
peKnobSpec = assertValid "pe knob" 16
    [ ("p0", SubSpec knob)
    , ("p1", SubSpec knob)
    , ("p2", SubSpec knob)
    , ("p3", SubSpec knob)
    ]
    where
    knob =
        -- TODO
        [ ("parameter", enum $ "off" : ["unknown " <> showt n | n <- [1..230]])
        , ("left value", percent)
        , ("right value", percent)
        , ("curve", enum ["linear", "exponential", "log", "sw"])
        ]


-- * multiset

-- | Spec to both parse and generate a multiset dump.
multisetSpec :: Specs
multisetSpec = Sysex.assertValid config "multiset_spec" 208
    [ ("name", Str 16)
    , ("timbre", List 6 timbreSpec)
    -- effect
    , ("effect 1", SubSpec
        [ ("select", enum effectType1)
        , ("setting", Unparsed 22)
        ])
    , ("effect 2", SubSpec
        [ ("effect 2 select", enum effectType2)
        , ("effect 2 setting", Unparsed 22)
        ])
    , ("master effect", SubSpec
        [ ("select", enum masterEffectType)
        , ("setting", Unparsed 18)
        , ("low eq freq", unsigned 49)
        , ("low eq gain", gain)
        , ("high eq freq", unsigned 49)
        , ("high eq gain", gain)
        ])
    -- common
    , pitchBend
    , ("", Bits
        [ ("scale key", enumBits 4 scaleKeys)
        , ("scale type", enumBits 4 scaleTypes)
        ])
    , ("", Bits
        [ ("portamento sw", boolBit)
        , ("unison sw", enumBits 7 ["off", "on"])
        ])
    , ("", Unparsed 10)
    , ("controller", SubSpec controllerSpec)
    , ("link arpeggio", SubSpec (linkArpeggioSpec 2))
    ]

timbreSpec :: Specs
timbreSpec =
    -- timbre
    [ ("", Bits
        [ ("program number", bits 7)
        , ("program bank", bits 1)
        ])
    , ("reserve voice", unsigned 12)
    -- pitch
    , ("transpose", ranged (-24) 24)
    , ("detune", ranged (-50) 50)
    , ("", Bits
        [ ("arpeggiator", (4, Sysex.Enum ["on", "off"]))
        , ("scale select", enumBits 4 ["common", "program"])
        ])
    -- mixer
    , ("output level", unsigned 128) -- 128 is PROG
    , ("panpot", unsigned 128) -- 128 is PROG
    , ("effect send", unsigned 101) -- 101 is PROG
    -- zone
    , ("key zone top", unsigned 127)
    , ("key zone bottom", unsigned 127)
    , ("velocity zone top", ranged 1 127)
    , ("velocity zone bottom", ranged 1 127)
    -- MIDI
    , ("", Bits
        [ ("midi channel", rangedBits 7 (0, 16)) -- 16 is GLOBAL
        , ("program change", boolBit)
        ])
    , ("", Bits
        [ ("realtime edit", boolBit)
        , ("portamento sw", boolBit)
        , ("damper", boolBit)
        , ("x-y pad controller", boolBit)
        , ("modulation wheel", boolBit)
        , ("after touch", boolBit)
        , ("pitch bend", enumBits 2 ["off", "common", "program"])
        ])
    , ("", Bits
        [ ("unused", bits 6)
        , ("performance edit", boolBit)
        , ("others", boolBit)
        ])
    , ("", Unparsed 1)
    ]

-- * effects

effectType2 :: [Sysex.EnumName]
effectType2 =
    [ "overdrive", "compressor", "parametric eq", "wah", "exciter"
    , "decimator", "chorus", "flanger", "phaser", "rotary speaker-small"
    , "delay-mono"
    ]

effectType1 :: [Sysex.EnumName]
effectType1 = effectType2 ++
    [ "talking modulator", "multitap delay", "ensemble", "rotary speaker-large"
    ]

effectOverdrive :: Specs
effectOverdrive =
    [ ("mode", enum ["overdrive", "distortion"])
    , ("drive", unsigned 99)
    , ("output level", unsigned 99)
    , ("pre low cutoff", unsigned 99)
    , eq "low", eq "mid low", eq "mid high", eq "high"
    , effectBalance
    ]
    where eq prefix = (prefix ++ " eq", SubSpec eqSettings)

effectCompressor :: Specs
effectCompressor =
    [ ("sensitivity", ranged 1 99)
    , ("attack", ranged 1 99)
    , ("eq trim", unsigned 99)
    , ("pre low eq gain", gain)
    , ("pre high eq gain", gain)
    , ("output level", unsigned 99)
    , effectBalance
    ]

effectParametricEq :: Specs
effectParametricEq =
    [ ("trim", unsigned 99)
    , ("low eq", SubSpec $
        ("type", enum ["peaking", "shelving"]) : eqSettings)
    , ("mid low eq", SubSpec $ eqSettings ++
        [ ("gain mod source", enum modSourceList2)
        , ("gain mod intensity", gain)
        ])
    , ("mid high eq", SubSpec eqSettings)
    , ("high eq", SubSpec $
        ("type", enum ["peaking", "shelving"]) : eqSettings)
    , effectBalance
    ]

effectWah :: Specs
effectWah =
    [ ("frequency bottom", unsigned 99)
    , ("frequency top", unsigned 99)
    , ("sweep mode", enum ["auto", "mod source"])
    , ("sweep source", enum modSourceList2)
    , ("sweep response", unsigned 99)
    , ("envelope sensitivity", unsigned 99)
    , ("envelope shape", signed 99)
    , ("resonance", unsigned 99)
    , ("filter mode", enum ["bpf", "lpf"])
    , effectBalance
    ]

effectExciter :: Specs
effectExciter =
    [ modSourceIntensity "blend" (signed 99)
    , modSourceIntensity "emphatic point" (signed 99)
    , ("pre eq input trim", unsigned 99)
    , ("pre low eq gain", gain)
    , ("pre high eq gain", gain)
    , effectBalance
    ]

effectDecimator :: Specs
effectDecimator =
    [ ("pre lpf", bool)
    , ("sampling frequency", ranged 5 120) -- 1.0k~24.0kHz
    , modSource2 "sampling frequency"
    -- Different from other intensities!
    , ("sampling frequency mod intensity", signed 120) -- -24.0k~+24.0kHz
    , ("resolution", ranged 4 24)
    , ("high damp", unsigned 99)
    , ("output level", unsigned 99)
    , effectBalance
    ]

effectChorus :: Specs
effectChorus =
    [ ("delay time", unsigned 140) -- 0-50 msec
    , ("lfo wave form", enum ["triangle", "sine"])
    , ("lfo frequency", ranged 1 115) -- 00.04~20.00Hz
    , modSource2 "lfo frequency"
    , ("lfo frequency mod intensity", signed 115) -- -20.00~+20.00Hz
    , midiSync
    , modSourceIntensity "depth" (unsigned 99)
    , ("pre eq input trim", unsigned 99)
    , ("pre low eq gain", gain)
    , ("pre high eq gain", gain)
    , effectBalance
    ]

effectFlanger :: Specs
effectFlanger =
    [ ("delay time", unsigned 140) -- 0.0~50.0msec
    , ("lfo", SubSpec
        [ ("wave form", enum ["triangle", "sine"])
        , ("shape", signed 99)
        , ("frequency", ranged 1 115) -- 00.04~20.00Hz
        , modSource2 "frequency"
        , ("frequency mod intensity", signed 115)
        ])
    , midiSync
    , ("depth", unsigned 99)
    , ("feedback", signed 99)
    , ("high damp", unsigned 99)
    , ("output phase", enum ["+", "-", "+-"])
    , effectBalance
    ]

effectPhaser :: Specs
effectPhaser =
    [ ("lfo", SubSpec
        [ ("wave form", enum ["triangle", "sine"])
        , ("shape", signed 99)
        , ("frequency", ranged 1 115) -- 00.04~20.00Hz
        , modSource2 "frequency"
        , ("frequency mod intensity", signed 115)
        ])
    , midiSync
    , ("manual", unsigned 99)
    , ("depth", unsigned 99)
    , ("resonance", signed 99)
    , ("high damp", unsigned 99)
    , ("output phase", enum ["+", "-", "+-"])
    , effectBalance
    ]

effectSmallRotarySpeaker :: Specs
effectSmallRotarySpeaker =
    [ ("speed", enum ["slow", "fast"])
    , ("speed sw", enum modSourceList2)
    , ("horn acceleration", unsigned 99)
    , ("horn rate", unsigned 99)
    , ("mic distance", unsigned 99)
    , ("horn / rotor balance", unsigned 99)
    , effectBalance
    ]

effectDelay :: Specs
effectDelay =
    [ modSource2 "input level"
    , modIntensity "input level"
    , ("delay time", delayTime)
    , ("feedback", signed 99)
    , modSource2 "feedback"
    , modIntensity "feedback"
    , ("low damp", unsigned 99)
    , ("high damp", unsigned 99)
    , effectBalance
    ]

effectTalkingModulator :: Specs
effectTalkingModulator =
    [ ("control mode", enum ["manual", "mod source"])
    , ("manual control", unsigned 99)
    , ("control source", enum modSourceList2)
    , ("voice top", enum ["a", "i", "u", "e", "o"])
    , ("voice center", enum ["a", "i", "u", "e", "o"])
    , ("voice bottom", enum ["a", "i", "u", "e", "o"])
    , ("formant shift", signed 99)
    , ("resonance", unsigned 99)
    , effectBalance
    ]

effectMultitapDelay :: Specs
effectMultitapDelay =
    [ ("type", enum ["normal", "cross1", "cross2", "pan1", "pan2"])
    , modSource2 "input level"
    , modIntensity "input level"
    , ("tap1 time", delayTime)
    , ("tap1 level", unsigned 99)
    , ("tap2 time", delayTime)
    , ("tap2 level", unsigned 99)
    , ("feedback", signed 99)
    , modSource2 "feedback"
    , modIntensity "feedback"
    , ("low damp", unsigned 99)
    , ("high damp", unsigned 99)
    , ("spread", signed 99)
    , modSource2 "spread"
    , modIntensity "spread"
    , effectBalance
    ]

effectEnsemble :: Specs
effectEnsemble =
    [ ("speed", unsigned 99)
    , modSource2 "speed"
    , modIntensity "speed"
    , ("shimmer", unsigned 99)
    , ("depth", unsigned 99)
    , modSource2 "depth"
    , modIntensity "depth"
    , effectBalance
    ]

effectRotarySpeaker :: Specs
effectRotarySpeaker =
    [ ("speed", enum ["slow", "fast"])
    , ("speed sw", enum modSourceList2)
    , ("mode", enum ["rotate", "stop"])
    , ("mode sw", enum modSourceList2)
    , ("rotor acceleration", unsigned 99)
    , ("rotor rate", unsigned 99)
    , ("horn acceleration", unsigned 99)
    , ("horn rate", unsigned 99)
    , ("mic distance", unsigned 99)
    , ("mic spread", unsigned 99)
    , ("horn / rotor balance", unsigned 99)
    , effectBalance
    ]

-- * master effects

effectStereoDelay :: Specs
effectStereoDelay =
    [ ("type", enum ["stereo", "cross"])
    , modSource2 "input level"
    , modIntensity "input level"
    , ("left delay time", delayTime)
    , ("right delay time", delayTime)
    , ("feedback l", signed 99)
    , ("feedback r", signed 99)
    , modSource2 "feedback"
    , modIntensity "feedback"
    , ("low damp", unsigned 99)
    , ("high damp", unsigned 99)
    , effectBalance
    ]

effectReverbHall :: Specs
effectReverbHall =
    [ ("reverb time", ranged 1 100) -- 0.1~10.0sec
    , ("pre delay", unsigned 200) -- msec
    , ("pre delay thru level", unsigned 99)
    , ("high damp", unsigned 99)
    , ("pre eq trim", unsigned 99)
    , ("pre low eq gain", gain)
    , ("pre high eq gain", gain)
    , effectBalance
    ]

effectReverbRoom :: Specs
effectReverbRoom =
    [ ("reverb time", ranged 1 30) -- 0.1~3.0sec
    , ("pre delay", unsigned 200) -- msec
    , ("pre delay thru level", unsigned 99)
    , ("high damp", unsigned 99)
    , ("er level", unsigned 99)
    , ("reverb level", unsigned 99)
    , ("pre eq trim", unsigned 99)
    , ("pre low eq gain", gain)
    , ("pre high eq gain", gain)
    ]

delayTime :: Spec
delayTime = unsigned 251 -- 0~680msec

-- * util

eqSettings :: Specs
eqSettings = [("freq", freq), ("q", unsigned 95), ("gain", gain)]

modSourceIntensity :: String -> Spec -> (String, Spec)
modSourceIntensity name range = (name, SubSpec
    [ ("val", range)
    , modSource2 name
    , modIntensity name
    ])

midiSync :: (Sysex.Name, Spec)
midiSync = ("midi sync", Bits
    [ ("time", rangedBits 4 (0, 15))
    , ("base", rangedBits 3 (0, 7)) -- 16th to whole
    , ("val", boolBit)
    ])

midiKey :: Spec
midiKey = unsigned 127

modSource1 :: String -> (String, Spec)
modSource1 name = (name <+> "mod source", enum modSourceList1)

modSource2 :: String -> (String, Spec)
modSource2 name = (name <+> "mod source", enum modSourceList2)

intensity :: String -> (String, Spec)
intensity name = (name <+> "intensity", signed 99)

modIntensity :: String -> (String, Spec)
modIntensity name = (name <+> "mod intensity", signed 99)

(<+>) :: String -> String -> String
x <+> y
    | null x = y
    | null y = x
    | otherwise = x ++ " " ++ y

-- | Gain in dB.
gain :: Spec
gain = ranged (-36) 36

bool :: Spec
bool = enum ["off", "on"]

-- | Frequency in Hz.
freq :: Spec
freq = unsigned 49

effectBalance :: (Sysex.Name, Spec)
effectBalance = ("effect balance", SubSpec
    [ ("balance", percent)
    , ("mod source", enum modSourceList2)
    , modIntensity ""
    ])

percent :: Spec
percent = unsigned 100

-- | TODO like mod_source_list_1 except entries 1--10 are invalid.
modSourceList2 :: [Sysex.EnumName]
modSourceList2 = modSourceList1

modSourceList1 :: [Sysex.EnumName]
modSourceList1 =
    [ "off"
    , "eg1", "eg2", "eg3", "eg4"
    , "amp eg"
    , "lfo1", "lfo2", "lfo3", "lfo4"
    , "portamento"
    , "note [linear]", "note [exp]"
    , "note split [high]", "note split [low]"
    , "velocity [soft]", "velocity [medium]", "velocity [hard]"
    , "pitch bend"
    , "after touch"
    , "modulation wheel (cc#1)"
    , "atouch + mod.wheel"
    , "athalf + mod.wheel"
    , "x [+/-] (cc#16)", "x [+] (cc#16)", "x [-] (cc#16)"
    , "y [+/-] (cc#17)", "y [+] (cc#17)", "y [-] (cc#17)"
    , "knob1 (cc#19)", "knob2 (cc#20)", "knob3 (cc#21)", "knob4 (cc#22)"
    , "knob5 (cc#23)"
    , "mod.sw1 (cc#80)", "mod.sw2 (cc#81)"
    , "foot sw (cc#82)"
    , "foot pedal (cc#4)"
    , "damper (cc#64)"
    , "sostenuto (cc#66)"
    , "midi breath control (cc#2)"
    , "midi volume (cc#7)"
    , "midi panpot (cc#10)"
    , "midi expression (cc#11)"
    , "midi portamento time (cc#5)"
    , "midi portamento sw (cc#65)"
    , "master fx off/on (cc#92)"
    , "fx1 off/on (cc#94)", "fx2 off/on (cc#95)"
    ]

masterEffectType :: [Sysex.EnumName]
masterEffectType = ["stereo delay", "reverb-hall", "reverb-room"]
