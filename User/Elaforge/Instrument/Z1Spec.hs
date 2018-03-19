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
    decode, encode, spec_bytes
    , program_dump_header, current_program_dump_header
    , patch_spec, multiset_spec
) where
import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import qualified Midi.Midi as Midi
import qualified Instrument.Sysex as Sysex
import Instrument.Sysex
       (Specs, Spec(..), bits, ranged_bits, unsigned, bool_bit, enum, enum_bits,
        ranged, signed)
import Global

-- TODO
-- * finish the effects
-- - put them in patch_spec and multiset_spec
-- - finish osc types

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
        [ ("scale key", enum_bits 4 scale_keys)
        , ("scale type", enum_bits 4 scale_types)
        ])
    , ("random pitch intensity", unsigned 99)
    , ("eg", List 4 eg_spec)
    , ("lfo", List 4 lfo_spec)
    -- osc common
    , pitch_bend
    , ("common pitch mod source", enum mod_source_list_1)
    , mod_intensity "common pitch"
    , ("portamento", SubSpec
        [ ("", Bits
            [ ("sw", bool_bit)
            , ("mode", enum_bits 7 ["normal", "fingered"])
            ])
        , ("time", unsigned 99)
        , ("mod source", enum mod_source_list_1)
        , mod_intensity ""
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
    , ("link arpeggio", SubSpec (link_arpeggio_spec 7))
    , ("pe knob", List 5 pe_knob_spec)
    ]

pitch_bend :: (Sysex.Name, Spec)
pitch_bend = ("pitch bend", SubSpec
    [ ("intensity +", ranged (-60) 24)
    , ("intensity -", ranged (-60) 24)
    , ("", Bits $
        let vals = ["0", "1/8", "1/4", "1/2"] ++ map showt [1..12] in
        [ ("step +", enum_bits 4 vals)
        , ("step -", enum_bits 4 vals)
        ])
    ])

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
    , mod_intensity "eg level"
    , ("eg level velocity control", signed 99)
    , mod_source_1 "eg time"
    , mod_intensity "eg time"
    , mod_source_1 "eg node time"
    , mod_intensity "attack time", mod_intensity "decay time"
    , mod_intensity "slope time", mod_intensity "release time"
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
    , mod_source_1 "frequency 1"
    , mod_intensity "frequency 1"
    , mod_source_1 "frequency 2"
    , mod_intensity "frequency 2"
    , ("fade in", unsigned 99)
    , mod_source_1 "amplitude"
    , mod_intensity "amplitude"
    , ("offset", signed 50)
    , ("", Bits
        [ ("midi sync time", ranged_bits 4 (0, 15))
        , ("midi sync base", ranged_bits 3 (0, 7))
        , ("midi sync", bool_bit)
        ])
    ]

lfo_waveforms :: [Sysex.EnumName]
lfo_waveforms =
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

scale_keys :: [Sysex.EnumName]
scale_keys =
    [ "c", "cs", "d", "ds", "e", "f", "g", "gs", "a", "as", "b"]

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
    , ("mod1 intensity controller intensity", signed 99)
    , ("mod2 source", enum mod_source_list_1)
    , ("mod2 intensity", signed 99)
    , ("setting", Unparsed 38) -- union of osc params
    ]

osc_types :: [Sysex.EnumName]
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
    , ("mod1 intensity", signed 99)
    , ("mod1 intensity controller", enum mod_source_list_1)
    , ("mod1 intensity controller intensity", signed 99)
    , ("mod2 source", enum mod_source_list_1)
    , ("mod2 intensity", signed 99)
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
    , ("cutoff mod1 intensity", signed 99)
    , mod_source_1 "cutoff mod2"
    , ("cutoff mod2 intensity", signed 99)
    , ("resonance", unsigned 99)
    ]

-- ** osc types

standard_osc :: Specs
standard_osc =
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
        , mod_intensity "lfo"
        , mod_source_1 ""
        , mod_intensity ""
        ])
    , ("wave shape", SubSpec
        [ ("input level", unsigned 99)
        , mod_source_1 "input level"
        , mod_intensity "input level"
        , ("offset", signed 99)
        , ("table", enum ["clip", "reso"])
        , ("wave shape", unsigned 99)
        , mod_source_1 ""
        , mod_intensity ""
        , ("balance", unsigned 99)
        , mod_source_1 "balance"
        , mod_intensity "balance"
        ])
    ]

comb_filter_osc :: Specs
comb_filter_osc =
    [ ("input select", enum ["sc1+noise", "subosc+noise", "filter1+noise",
        "filter2+noise", "pulse noise", "impulse"])
    , ("input wave level", unsigned 99)
    , ("noise level", unsigned 99)
    , ("width", unsigned 99)
    , mod_source_1 "input level"
    , mod_intensity "input level"
    , ("comb filter feedback", unsigned 99)
    , mod_source_1 "feedback 1"
    , mod_intensity "feedback 1"
    , mod_source_1 "feedback 2"
    , mod_intensity "feedback 2"
    , ("high damp", unsigned 99)
    , mod_source_1 "high damp"
    , mod_intensity "high damp"
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
            , mod_intensity "level"
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
            , mod_intensity "eg"
            , mod_source_1 "1", mod_intensity "1"
            , mod_source_1 "2", mod_intensity "2"
            ])
        -- resonance
        , ("resonance", unsigned 99)
        , mod_source_1 "resonance"
        , mod_intensity "resonance"
        ])
    , ("b", SubSpec $ filter_common ++
        [ ("cutoff frequency", SubSpec
            [ intensity "mod eg"
            , intensity "mod1"
            , intensity "mod2"
            ])
        , ("resonance", unsigned 99)
        , mod_intensity "resonance mod"
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
        , mod_intensity ""
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
        , mod_intensity "eg level"
        , ("eg velocity control", signed 99)
        , mod_source_1 "eg time"
        , mod_intensity "eg time"
        , mod_source_1 "eg node time"
        , mod_intensity "attack time"
        , mod_intensity "decay time"
        , mod_intensity "slope time"
        , mod_intensity "release time"
        ])
    ]
    where
    level name = (name <+> "level", signed 99)
    time name = (name <+> "time", unsigned 99)

output_spec :: Specs
output_spec = assert_valid "output" 4
    [ ("panpot", unsigned 127)
    , mod_source_1 "panpot"
    , mod_intensity "panpot"
    , ("output", unsigned 127)
    ]

effect_spec :: Specs
effect_spec = assert_valid "effect" 72
    [ ("effect send", unsigned 100)
    , mod_source_1 "effect send"
    , mod_intensity "effect send"
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

link_arpeggio_spec :: Int -> Specs
link_arpeggio_spec reserved = assert_valid "link arpeggio" (6 + reserved)
    [ ("unparsed", Unparsed 6) -- TODO
    , ("", Unparsed reserved)
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
        [ ("parameter", enum $ "off" : ["unknown " <> showt n | n <- [1..230]])
        , ("left value", percent)
        , ("right value", percent)
        , ("curve", enum ["linear", "exponential", "log", "sw"])
        ]


-- * multiset

-- | Spec to both parse and generate a multiset dump.
multiset_spec :: Specs
multiset_spec = Sysex.assert_valid config "multiset_spec" 208
    [ ("name", Str 16)
    , ("timbre", List 6 timbre_spec)
    -- effect
    , ("effect 1", SubSpec
        [ ("select", enum effect_type1)
        , ("setting", Unparsed 22)
        ])
    , ("effect 2", SubSpec
        [ ("effect 2 select", enum effect_type2)
        , ("effect 2 setting", Unparsed 22)
        ])
    , ("master effect", SubSpec
        [ ("select", enum master_effect_type)
        , ("setting", Unparsed 18)
        , ("low eq freq", unsigned 49)
        , ("low eq gain", gain)
        , ("high eq freq", unsigned 49)
        , ("high eq gain", gain)
        ])
    -- common
    , pitch_bend
    , ("", Bits
        [ ("scale key", enum_bits 4 scale_keys)
        , ("scale type", enum_bits 4 scale_types)
        ])
    , ("", Bits
        [ ("portamento sw", bool_bit)
        , ("unison sw", enum_bits 7 ["off", "on"])
        ])
    , ("", Unparsed 10)
    , ("controller", SubSpec controller_spec)
    , ("link arpeggio", SubSpec (link_arpeggio_spec 2))
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

effect_type2 :: [Sysex.EnumName]
effect_type2 =
    [ "overdrive", "compressor", "parametric eq", "wah", "exciter"
    , "decimator", "chorus", "flanger", "phaser", "rotary speaker-small"
    , "delay-mono"
    ]

effect_type1 :: [Sysex.EnumName]
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

effect_wah :: Specs
effect_wah =
    [ ("frequency bottom", unsigned 99)
    , ("frequency top", unsigned 99)
    , ("sweep mode", enum ["auto", "mod source"])
    , ("sweep source", enum mod_source_list_2)
    , ("sweep response", unsigned 99)
    , ("envelope sensitivity", unsigned 99)
    , ("envelope shape", signed 99)
    , ("resonance", unsigned 99)
    , ("filter mode", enum ["bpf", "lpf"])
    , effect_balance
    ]

effect_exciter :: Specs
effect_exciter =
    [ mod_source_intensity "blend" (signed 99)
    , mod_source_intensity "emphatic point" (signed 99)
    , ("pre eq input trim", unsigned 99)
    , ("pre low eq gain", gain)
    , ("pre high eq gain", gain)
    , effect_balance
    ]

effect_decimator :: Specs
effect_decimator =
    [ ("pre lpf", bool)
    , ("sampling frequency", ranged 5 120) -- 1.0k~24.0kHz
    , mod_source_2 "sampling frequency"
    -- Different from other intensities!
    , ("sampling frequency mod intensity", signed 120) -- -24.0k~+24.0kHz
    , ("resolution", ranged 4 24)
    , ("high damp", unsigned 99)
    , ("output level", unsigned 99)
    , effect_balance
    ]

effect_chorus :: Specs
effect_chorus =
    [ ("delay time", unsigned 140) -- 0-50 msec
    , ("lfo wave form", enum ["triangle", "sine"])
    , ("lfo frequency", ranged 1 115) -- 00.04~20.00Hz
    , mod_source_2 "lfo frequency"
    , ("lfo frequency mod intensity", signed 115) -- -20.00~+20.00Hz
    , midi_sync
    , mod_source_intensity "depth" (unsigned 99)
    , ("pre eq input trim", unsigned 99)
    , ("pre low eq gain", gain)
    , ("pre high eq gain", gain)
    , effect_balance
    ]

effect_flanger :: Specs
effect_flanger =
    [ ("delay time", unsigned 140) -- 0.0~50.0msec
    , ("lfo", SubSpec
        [ ("wave form", enum ["triangle", "sine"])
        , ("shape", signed 99)
        , ("frequency", ranged 1 115) -- 00.04~20.00Hz
        , mod_source_2 "frequency"
        , ("frequency mod intensity", signed 115)
        ])
    , midi_sync
    , ("depth", unsigned 99)
    , ("feedback", signed 99)
    , ("high damp", unsigned 99)
    , ("output phase", enum ["+", "-", "+-"])
    , effect_balance
    ]

effect_phaser :: Specs
effect_phaser =
    [ ("lfo", SubSpec
        [ ("wave form", enum ["triangle", "sine"])
        , ("shape", signed 99)
        , ("frequency", ranged 1 115) -- 00.04~20.00Hz
        , mod_source_2 "frequency"
        , ("frequency mod intensity", signed 115)
        ])
    , midi_sync
    , ("manual", unsigned 99)
    , ("depth", unsigned 99)
    , ("resonance", signed 99)
    , ("high damp", unsigned 99)
    , ("output phase", enum ["+", "-", "+-"])
    , effect_balance
    ]

effect_small_rotary_speaker :: Specs
effect_small_rotary_speaker =
    [ ("speed", enum ["slow", "fast"])
    , ("speed sw", enum mod_source_list_2)
    , ("horn acceleration", unsigned 99)
    , ("horn rate", unsigned 99)
    , ("mic distance", unsigned 99)
    , ("horn / rotor balance", unsigned 99)
    , effect_balance
    ]

effect_delay :: Specs
effect_delay =
    [ mod_source_2 "input level"
    , mod_intensity "input level"
    , ("delay time", delay_time)
    , ("feedback", signed 99)
    , mod_source_2 "feedback"
    , mod_intensity "feedback"
    , ("low damp", unsigned 99)
    , ("high damp", unsigned 99)
    , effect_balance
    ]

effect_talking_modulator :: Specs
effect_talking_modulator =
    [ ("control mode", enum ["manual", "mod source"])
    , ("manual control", unsigned 99)
    , ("control source", enum mod_source_list_2)
    , ("voice top", enum ["a", "i", "u", "e", "o"])
    , ("voice center", enum ["a", "i", "u", "e", "o"])
    , ("voice bottom", enum ["a", "i", "u", "e", "o"])
    , ("formant shift", signed 99)
    , ("resonance", unsigned 99)
    , effect_balance
    ]

effect_multitap_delay :: Specs
effect_multitap_delay =
    [ ("type", enum ["normal", "cross1", "cross2", "pan1", "pan2"])
    , mod_source_2 "input level"
    , mod_intensity "input level"
    , ("tap1 time", delay_time)
    , ("tap1 level", unsigned 99)
    , ("tap2 time", delay_time)
    , ("tap2 level", unsigned 99)
    , ("feedback", signed 99)
    , mod_source_2 "feedback"
    , mod_intensity "feedback"
    , ("low damp", unsigned 99)
    , ("high damp", unsigned 99)
    , ("spread", signed 99)
    , mod_source_2 "spread"
    , mod_intensity "spread"
    , effect_balance
    ]

effect_ensemble :: Specs
effect_ensemble =
    [ ("speed", unsigned 99)
    , mod_source_2 "speed"
    , mod_intensity "speed"
    , ("shimmer", unsigned 99)
    , ("depth", unsigned 99)
    , mod_source_2 "depth"
    , mod_intensity "depth"
    , effect_balance
    ]

effect_rotary_speaker :: Specs
effect_rotary_speaker =
    [ ("speed", enum ["slow", "fast"])
    , ("speed sw", enum mod_source_list_2)
    , ("mode", enum ["rotate", "stop"])
    , ("mode sw", enum mod_source_list_2)
    , ("rotor acceleration", unsigned 99)
    , ("rotor rate", unsigned 99)
    , ("horn acceleration", unsigned 99)
    , ("horn rate", unsigned 99)
    , ("mic distance", unsigned 99)
    , ("mic spread", unsigned 99)
    , ("horn / rotor balance", unsigned 99)
    , effect_balance
    ]

-- * master effects

effect_stereo_delay :: Specs
effect_stereo_delay =
    [ ("type", enum ["stereo", "cross"])
    , mod_source_2 "input level"
    , mod_intensity "input level"
    , ("left delay time", delay_time)
    , ("right delay time", delay_time)
    , ("feedback l", signed 99)
    , ("feedback r", signed 99)
    , mod_source_2 "feedback"
    , mod_intensity "feedback"
    , ("low damp", unsigned 99)
    , ("high damp", unsigned 99)
    , effect_balance
    ]

effect_reverb_hall :: Specs
effect_reverb_hall =
    [ ("reverb time", ranged 1 100) -- 0.1~10.0sec
    , ("pre delay", unsigned 200) -- msec
    , ("pre delay thru level", unsigned 99)
    , ("high damp", unsigned 99)
    , ("pre eq trim", unsigned 99)
    , ("pre low eq gain", gain)
    , ("pre high eq gain", gain)
    , effect_balance
    ]

effect_reverb_room :: Specs
effect_reverb_room =
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

delay_time :: Spec
delay_time = unsigned 251 -- 0~680msec

-- * util

eq_settings :: Specs
eq_settings = [("freq", freq), ("q", unsigned 95), ("gain", gain)]

mod_source_intensity :: String -> Spec -> (String, Spec)
mod_source_intensity name range = (name, SubSpec
    [ ("val", range)
    , mod_source_2 name
    , mod_intensity name
    ])

midi_sync :: (Sysex.Name, Spec)
midi_sync = ("midi sync", Bits
    [ ("time", ranged_bits 4 (0, 15))
    , ("base", ranged_bits 3 (0, 7)) -- 16th to whole
    , ("val", bool_bit)
    ])

midi_key :: Spec
midi_key = unsigned 127

mod_source_1 :: String -> (String, Spec)
mod_source_1 name = (name <+> "mod source", enum mod_source_list_1)

mod_source_2 :: String -> (String, Spec)
mod_source_2 name = (name <+> "mod source", enum mod_source_list_2)

intensity :: String -> (String, Spec)
intensity name = (name <+> "intensity", signed 99)

mod_intensity :: String -> (String, Spec)
mod_intensity name = (name <+> "mod intensity", signed 99)

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

effect_balance :: (Sysex.Name, Spec)
effect_balance = ("effect balance", SubSpec
    [ ("balance", percent)
    , ("mod source", enum mod_source_list_2)
    , mod_intensity ""
    ])

percent :: Spec
percent = unsigned 100

-- | TODO like mod_source_list_1 except entries 1--10 are invalid.
mod_source_list_2 :: [Sysex.EnumName]
mod_source_list_2 = mod_source_list_1

mod_source_list_1 :: [Sysex.EnumName]
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

master_effect_type :: [Sysex.EnumName]
master_effect_type = ["stereo delay", "reverb-hall", "reverb-room"]
