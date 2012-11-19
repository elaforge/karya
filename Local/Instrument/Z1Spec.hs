module Local.Instrument.Z1Spec (
    decode, encode, spec_bytes
    , program_dump_header, current_program_dump_header
    , patch_spec, multiset_spec
) where
import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import qualified Midi.Midi as Midi
import qualified Instrument.Sysex as Sysex
import Instrument.Sysex
       (Specs, Spec(..), bits, ranged_bits, unsigned, bool_bit, enum,
        enum_bits, ranged, signed)


config :: Sysex.Config
config = Sysex.config_8bit

encode :: Specs -> Sysex.RMap -> Either String ByteString
encode = Sysex.encode config

decode :: Specs -> ByteString -> Either String (Sysex.RMap, ByteString)
decode = Sysex.decode config

spec_bytes :: Specs -> Int
spec_bytes = Sysex.spec_bytes config

assert_valid :: String -> Int -> Specs -> Specs
assert_valid = Sysex.assert_valid config

-- | I think 0x30 should be 0x3g where g is the global channel, but I use 0
-- so this works.
z1_header :: ByteString
z1_header = B.pack [0xf0, Midi.korg_code, 0x30, 0x46]

program_dump_header :: Specs
program_dump_header =
    [ ("", Constant z1_header)
    , ("", Constant (B.singleton 0x4c))
    , ("", Bits
        [ ("bank", enum_bits 4 ["a", "b"])
        , ("unit", enum_bits 4 ["program", "bank", "", "all"])
        ])
    , ("program number", unsigned 127)
    , ("", Constant (B.singleton 0))
    ]

current_program_dump_header :: Specs
current_program_dump_header =
    [ ("", Constant z1_header)
    , ("", Constant $ B.pack [0x40, 0x01])
    ]

-- * program / patch

patch_spec :: Specs
patch_spec = Sysex.assert_valid config "patch_spec" 576
    [ ("name", Str 16)
    , ("category", enum categories)
    , ("user group", unsigned 15)
    , ("", Bits
        [ ("hold", bits 1)
        , ("key priority", enum_bits 2 ["last", "low", "high"])
        , ("voice assign mode", enum_bits 5
            ["mono multi", "mono single", "poly"])
        ])
    , ("retrigger controllor", enum mod_source_list_2)
    , ("retrigger control threshold", ranged 1 127)
    , ("", Bits
        [ ("unison type", enum_bits 2 ["off", "2", "3", "6"])
        , ("unison sw", bool_bit)
        , ("unison mode", enum_bits 5 ["fixed", "dynamic"])
        ])
    , ("unison detune", unsigned 99)
    -- scale
    , ("", Bits
        [ ("scale key", ranged_bits 4 (0, 11))
        , ("scale type", enum_bits 4 scale_types)
        ])
    , ("random pitch intensity", unsigned 99)
    , ("eg", List 4 eg_spec)
    , ("lfo", List 4 lfo_spec)
    -- osc common
    , ("pitch bend", SubSpec
        [ ("intensity +", ranged (-60) 24)
        , ("intensity -", ranged (-60) 24)
        , ("", Bits $
            let vals = ["0", "1/8", "1/4", "1/2"] ++ map show [1..12] in
            [ ("step +", enum_bits 4 vals)
            , ("step -", enum_bits 4 vals)
            ])
        ])
    , ("common pitch mod source", enum mod_source_list_1)
    , intensity "common pitch mod"
    , ("portamento", SubSpec
        [ ("", Bits
            [ ("sw", bool_bit)
            , ("mode", enum_bits 7 ["normal", "fingered"])
            ])
        , ("time", unsigned 99)
        , ("mod source", enum mod_source_list_1)
        , intensity "mod"
        ])
    , ("osc", List 2 osc_spec)
    , ("sub osc", SubSpec sub_osc_spec)
    , ("noise generator filter", SubSpec noise_generator_spec)
    , ("mixer", SubSpec mixer_spec)
    , ("filter common", SubSpec filter_common_spec)
    , ("filter", List 2 filter_spec)
    , ("amp", SubSpec amp_spec)
    , ("output", SubSpec output_spec)
    , ("effect", SubSpec effect_spec)
    , ("controller", SubSpec controller_spec)
    , ("link arpeggio", SubSpec link_arpeggio_spec)
    , ("pe knob", List 5 pe_knob_spec)
    ]

eg_spec :: Specs
eg_spec =
    [ level "start"
    , time "attack"
    , level "attack"
    , time "decay"
    , level "break"
    , time "slope"
    , level "sustain"
    , time "release"
    , level "release"
    , mod_source_1 "eg level"
    , intensity "eg level"
    , ("eg level velocity control", signed 99)
    , mod_source_1 "eg time"
    , intensity "eg time"
    , mod_source_1 "eg node time"
    , intensity "attack time", intensity "decay time", intensity "slope time"
    , intensity "release time"
    ]
    where
    level name = (name <+> "level", signed 99)
    time name = (name <+> "time", unsigned 99)

lfo_spec :: Specs
lfo_spec =
    [ ("", Bits
        [ ("wave form", enum_bits 6 lfo_waveforms)
        , ("key sync sw", enum_bits 2 ["off", "timbre", "voice"])
        ])
    , ("frequency", unsigned 199)
    , mod_source_1 "frequency mod1"
    , intensity "frequency mod1"
    , mod_source_1 "frequency mod2"
    , intensity "frequency mod2"
    , ("fade in", unsigned 99)
    , mod_source_1 "amplitude mod"
    , intensity "amplitude mod"
    , ("offset", signed 50)
    , ("", Bits
        [ ("midi sync time", ranged_bits 4 (0, 15))
        , ("midi sync base", ranged_bits 3 (0, 7))
        , ("midi sync", bool_bit)
        ])
    ]

lfo_waveforms :: [String]
lfo_waveforms =
    [ "triangle 0", "triangle 90", "tri random", "sine"
    , "saw up 0", "saw up 180", "saw down 0", "saw down 180"
    , "square", "random s/h", "random vector"
    , "step tri 4", "step tri 6", "step saw 4", "step saw 6"
    , "exp tri", "exp saw up", "exp saw down"
    ]

-- | The Z1 has a built-in set of categories. Map the category index to the
-- name.
categories :: [String]
categories =
    [ "Synth-Hard", "Synth-Soft", "Synth-Lead", "Synth-Motion", "Synth-Bass"
    , "E.Piano", "Organ", "Keyboard", "Bell", "Strings", "Band/Choir"
    , "Brass", "Reed/Wind", "Guitar/Plucked", "Bass", "Percussive"
    , "Argpeggio", "SFX/Other"
    ]

scale_types :: [Sysex.EnumName]
scale_types =
    [ "equal temperament", "pure major temperament", "pure minor temperament"
    , "arabic temperament", "pythagorean", "werckmeister", "kirnberger"
    , "slendro", "pelog", "user scale 1", "user scale 2", "user scale 3"
    ]

-- * osc

osc_spec :: Specs
osc_spec =
    [ ("type", enum osc_types)
    , ("octave", octave)
    , ("semi tone", signed 12)
    , ("fine tune", signed 50)
    , ("frequency offset", signed 100)
    -- pitch slope
    , ("center key", unsigned 127)
    , ("lower slope", ranged (-50) 100)
    , ("higher slope", ranged (-50) 100)
    -- pitch modulation
    , ("mod1 source", enum mod_source_list_1)
    , intensity "mod1"
    , ("mod1 intensity controller", enum mod_source_list_1)
    , intensity "mod1 intensity controller"
    , ("mod2 source", enum mod_source_list_1)
    , intensity "mod2"
    , ("setting", Unparsed 38) -- union of osc params
    ]

osc_types :: [String]
osc_types =
    [ "standard", "comb", "vpm", "resonance", "ring-mod", "cross-mod", "sync"
    , "organ", "electric-piano", "brass", "reed", "plucked", "bowed"
    ]

sub_osc_spec :: Specs
sub_osc_spec =
    [ ("octave", octave)
    , ("semi tone", signed 12)
    , ("fine tune", signed 50)
    , ("frequency offset", signed 100)
    -- pitch slope
    , ("center key", unsigned 127)
    , ("lower slope", ranged (-50) 100)
    , ("higher slope", ranged (-50) 100)
    -- pitch modulation
    , ("mod1 source", enum mod_source_list_1)
    , intensity "mod1"
    , ("mod1 intensity controller", enum mod_source_list_1)
    , intensity "mod1 intensity controller"
    , ("mod2 source", enum mod_source_list_1)
    , intensity "mod2"
    , ("wave form", enum ["saw", "square", "triangle", "sine"])
    ]

octave :: Spec
octave = enum ["-2", "-1", "0", "1"] -- 32', 16', 8', 4'

noise_generator_spec :: Specs
noise_generator_spec =
    [ ("type", enum ["thru", "lpf", "hpf", "bpf"])
    , ("input trim", unsigned 99)
    , ("cutoff", unsigned 99)
    , mod_source_1 "cutoff mod1"
    , intensity "cutoff mod1"
    , mod_source_1 "cutoff mod2"
    , intensity "cutoff mod2"
    , ("resonance", unsigned 99)
    ]

-- * mixer

mixer_spec :: Specs
mixer_spec =
    [ channel "osc1 out1", channel "osc1 out2"
    , channel "osc2 out1", channel "osc2 out2"
    , channel "sub osc out1", channel "sub osc out2"
    , channel "noise out1", channel "noise out2"
    , channel "feedback out1", channel "feedback out2"
    , ("", Bits
        [ ("osc1 mixer output sw", bool_bit)
        , ("osc2 mixer output sw", bool_bit)
        , ("sub mixer output sw", bool_bit)
        , ("noise mixer output sw", bool_bit)
        , Sysex.unparsed_bits 4
        ])
    ]
    where
    channel name =
        (name, SubSpec
            [ ("level", unsigned 99)
            , mod_source_1 "level"
            , intensity "level mod"
            ])

-- * filter

filter_common_spec :: Specs
filter_common_spec =
    [ ("", Bits
        [ ("filter routing", enum_bits 2 ["serial1", "serial2", "parallel"])
        , ("filter2 link switch", enum_bits 2 ["off", "on"])
        , ("filter eg knob target", enum_bits 5
            ["", "filter1", "filter2", "both"])
        ])
    ]

filter_spec :: Specs
filter_spec =
    [ ("filter type", enum ["", "lpf", "hpf", "bpf", "brf", "2bpf"])
    , ("a", SubSpec $ filter_common ++
        [ ("cutoff frequency", SubSpec
            [ ("mod eg", egs)
            , intensity "mod eg"
            , ("mod1 source", enum mod_source_list_1), intensity "mod1"
            , ("mod2 source", enum mod_source_list_1), intensity "mod2"
            ])
        -- resonance
        , ("resonance", unsigned 99)
        , mod_source_1 "resonance"
        , intensity "resonance mod"
        ])
    , ("b", SubSpec $ filter_common ++
        [ ("cutoff frequency", SubSpec
            [ intensity "mod eg"
            , intensity "mod1"
            , intensity "mod2"
            ])
        , ("resonance", unsigned 99)
        , intensity "resonance mod"
        ])
    ]
    where
    filter_common =
        [ ("input trim", unsigned 99)
        , ("cutoff frequency", unsigned 99)
        -- cutoff keyboard track
        , ("low key", midi_key), ("high key", midi_key)
        , intensity "lower", intensity "higher"
        ]

egs :: Spec
egs = enum ["", "eg1", "eg2", "eg3", "eg4", "amp eg"]
    -- TODO 0 is undocumented but I assume off?  Or invalid?

-- * other

amp_spec :: Specs
amp_spec = assert_valid "amp" 37
    [ ("amp", List 2
        [ ("amplitude", unsigned 99)
        , ("low key", midi_key)
        , ("high key", midi_key)
        , intensity "lower"
        , intensity "higher"
        , ("mod eg", egs)
        , ("", Unparsed 1)
        , mod_source_1 ""
        , intensity "mod"
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
        , mod_source_1 "eg level"
        , intensity "eg level mod"
        , ("eg velocity control", signed 99)
        , mod_source_1 "eg time"
        , intensity "eg time mod"
        , mod_source_1 "eg node time"
        , intensity "attack time mod"
        , intensity "decay time mod"
        , intensity "slope time mod"
        , intensity "release time mod"
        ])
    ]
    where
    level name = (name <+> "level", signed 99)
    time name = (name <+> "time", unsigned 99)

output_spec :: Specs
output_spec = assert_valid "output" 4
    [ ("panpot", unsigned 127)
    , mod_source_1 "panpot"
    , intensity "panpot mod"
    , ("output", unsigned 127)
    ]

effect_spec :: Specs
effect_spec = assert_valid "effect" 72
    [ ("effect send", unsigned 100)
    , mod_source_1 "effect send"
    , intensity "effect send mod"
    , ("effect1 select", enum effect_type1)
    , ("effect1 setting", Unparsed 22)
    , ("effect2 select", enum effect_type1)
    , ("effect2 setting", Unparsed 22)
    , ("master effect select", enum master_effect_type)
    , ("master effect setting", Unparsed 18)
    , ("eq", SubSpec
        [ ("low freq", freq)
        , ("low gain", gain)
        , ("high freq", freq)
        , ("high gain", gain)
        ])
    ]

controller_spec :: Specs
controller_spec = assert_valid "controller" 4
    [ ("", Bits
        [ ("sw1 function", enum_bits 7 sw1_function)
        , ("sw1 mode", latch)
        ])
    , ("", Bits
        [ ("sw2 function", enum_bits 7 sw2_function)
        , ("sw2 mode", latch)
        ])
    , ("", Bits
        [ ("foot sw function", enum_bits 7 foot_sw_function)
        , ("foot sw mode", latch)
        ])
    , ("", Bits
        [ ("pedal function", enum_bits 3 pedal_function)
        , ("pad hold", enum_bits 5 ["off", "on"])
        ])
    ]
    where
    latch = enum_bits 1 ["latch", "unlatch"]
    sw1_function =
        [ "mod sw 1", "master fx", "fx1", "fx2", "octave up", "octave down"
        , "mono sw", "unison"
        ]
    sw2_function =
        [ "mod sw 2", "master fx", "fx1", "fx2", "octave up", "octave down"
        , "mono sw", "unison"
        ]
    foot_sw_function =
        [ "foot sw", "midi damper", "midi portamento", "midi sostenuto"
        , "master fx", "fx1", "fx2", "octave up", "octave down", "mono sw"
        , "unison", "arpeggio", "pad hold"
        ]
    pedal_function =
        [ "pedal", "breath control", "portamento time", "volume", "panpot"
        , "expression"
        ]

link_arpeggio_spec :: Specs
link_arpeggio_spec = assert_valid "link arpeggio" 13
    [ ("unparsed", Unparsed 13) -- TODO
    ]

pe_knob_spec :: Specs
pe_knob_spec = assert_valid "pe knob" 16
    [ ("p0", SubSpec knob)
    , ("p1", SubSpec knob)
    , ("p2", SubSpec knob)
    , ("p3", SubSpec knob)
    ]
    where
    knob =
        -- TODO
        [ ("parameter", enum $ "off" : ["unknown " ++ show n | n <- [1..230]])
        , ("left value", percent)
        , ("right value", percent)
        , ("curve", enum ["linear", "exponential", "log", "sw"])
        ]

-- * util

midi_key :: Spec
midi_key = unsigned 127

mod_source_1 :: String -> (String, Spec)
mod_source_1 name = (name <+> "mod source", enum mod_source_list_1)

intensity :: String -> (String, Spec)
intensity name = (name <+> "intensity", signed 99)

percent :: Spec
percent = unsigned 100

(<+>) :: String -> String -> String
x <+> y
    | null x = y
    | null y = x
    | otherwise = x ++ " " ++ y

-- * multiset

-- | Spec to both parse and generate a multiset dump.
multiset_spec :: Specs
multiset_spec = Sysex.assert_valid config "multiset_spec" 208
    [ ("name", Str 16)
    , ("timbre", List 6 timbre_spec)
    , ("effect 1 select", enum effect_type1)
    , ("effect 1 setting", Unparsed 22)
    -- , Union "effect 1 setting" "effect 1 select" 22 $ zip effect_type1
    --     [ effect_overdrive, effect_compressor, effect_parametric_eq
    --     , [], [], [], [], [], [], [], []
    --     ]
    , ("effect 2 select", enum effect_type2)
    , ("effect 2 setting", Unparsed 22)
    -- , Union "effect 2 setting" "effect 2 select" 22 $ zip effect_type2 []
    , ("master effect select", enum master_effect_type)
    , ("master effect setting", Unparsed 18)
    -- , Union "master effect setting" "master effect select" 18 $
    --     zip master_effect_type
    --     []
    -- TODO
    ]

timbre_spec :: Specs
timbre_spec =
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
        , ("scale select", enum_bits 4 ["common", "program"])
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
        [ ("midi channel", ranged_bits 7 (0, 16)) -- 16 is GLOBAL
        , ("program change", bool_bit)
        ])
    , ("", Bits
        [ ("realtime edit", bool_bit)
        , ("portamento sw", bool_bit)
        , ("damper", bool_bit)
        , ("x-y pad controller", bool_bit)
        , ("modulation wheel", bool_bit)
        , ("after touch", bool_bit)
        , ("pitch bend", enum_bits 2 ["off", "common", "program"])
        ])
    , ("", Bits
        [ ("unused", bits 6)
        , ("performance edit", bool_bit)
        , ("others", bool_bit)
        ])
    , ("", Unparsed 1)
    ]

-- * effects

effect_type2 :: [String]
effect_type2 =
    [ "overdrive", "compressor", "parametric eq", "wah", "exciter"
    , "decimator", "chorus", "flanger", "phaser", "rotary speaker-small"
    , "delay-mono"
    ]

effect_type1 :: [String]
effect_type1 = effect_type2 ++
    [ "talking modulator", "multitap delay", "ensemble", "rotary speaker-large"
    ]

effect_overdrive :: Specs
effect_overdrive =
    [ ("mode", enum ["overdrive", "distortion"])
    , ("drive", unsigned 99)
    , ("output level", unsigned 99)
    , ("pre low cutoff", unsigned 99)
    , eq "low", eq "mid low", eq "mid high", eq "high"
    , effect_balance
    ]
    where eq prefix = (prefix ++ " eq", SubSpec eq_settings)

effect_compressor :: Specs
effect_compressor =
    [ ("sensitivity", ranged 1 99)
    , ("attack", ranged 1 99)
    , ("eq trim", unsigned 99)
    , ("pre low eq gain", gain)
    , ("pre high eq gain", gain)
    , ("output level", unsigned 99)
    , effect_balance
    ]

effect_parametric_eq :: Specs
effect_parametric_eq =
    [ ("trim", unsigned 99)
    , ("low eq", SubSpec $
        ("type", enum ["peaking", "shelving"]) : eq_settings)
    , ("mid low eq", SubSpec $ eq_settings ++
        [ ("gain mod source", enum mod_source_list_2)
        , ("gain mod intensity", gain)
        ])
    , ("mid high eq", SubSpec eq_settings)
    , ("high eq", SubSpec $
        ("type", enum ["peaking", "shelving"]) : eq_settings)
    , effect_balance
    ]

eq_settings :: Specs
eq_settings = [("freq", freq), ("q", unsigned 95), ("gain", gain)]

-- | Gain in dB.
gain :: Spec
gain = ranged (-36) 36

-- | Frequency in Hz.
freq :: Spec
freq = unsigned 49

effect_balance :: (Sysex.Name, Spec)
effect_balance = ("effect balance", SubSpec
    [ ("balance", unsigned 100)
    , ("effect balance mod source", enum mod_source_list_2)
    , intensity "effect balance mod"
    ])

-- | TODO like mod_source_list_1 except entries 1--10 are invalid.
mod_source_list_2 :: [String]
mod_source_list_2 = mod_source_list_1

mod_source_list_1 :: [String]
mod_source_list_1 =
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

master_effect_type :: [String]
master_effect_type = ["stereo delay", "reverb-hall", "reverb-room"]
