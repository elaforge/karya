-- | Korg Z1 keyboard.
module Local.Instrument.Z1 where
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import System.FilePath ((</>))

import Util.Control
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Parse as Parse
import qualified Instrument.Sysex as Sysex
import Instrument.Sysex
       (Spec(..), bits, ranged_bits, byte, boolean, boolean1, enum_byte,
        enum_bits, reserved_space, ranged_byte)

import qualified App.MidiInst as MidiInst


name :: FilePath
name = "z1"

load :: FilePath -> IO [MidiInst.SynthDesc]
load = MidiInst.load_db (const MidiInst.empty_code) name

make_db :: FilePath -> IO ()
make_db dir = do
    patches <- (++) <$> Parse.patch_file (dir </> name)
        <*> Sysex.parse_dir parse_patch (dir </> "z1_sysex")
    MidiInst.save_patches synth patches name dir

synth :: Instrument.Synth
synth = Instrument.synth name synth_controls

synth_controls :: [(Midi.Control, String)]
synth_controls =
    -- The PE controls are the "performance expression" knobs whose effect
    -- depends on the instrument.
    [ (19, "knob1"), (20, "knob2"), (21, "knob3"), (22, "knob4"), (23, "knob5")
    , (16, "pad-x"), (17, "pad-y")
    , (65, "port-sw") -- Turn portamento on and off.
    , (80, "sw1"), (81, "sw2") -- General purpose on/off switches.
    -- filter 1
    , (85, "filter1-cutoff"), (86, "filter1-q"), (87, "filter1-eg")
    , (24, "filter1-attack"), (25, "filter1-decay"), (26, "filter1-sustain")
    , (27, "filter1-release")
    -- filter 2
    , (88, "filter2-cutoff"), (89, "filter2-q"), (90, "filter2-eg")
    , (28, "filter2-attack"), (29, "filter2-decay"), (30, "filter2-sustain")
    , (31, "filter2-release")
    -- amp
    , (76, "amp-attack"), (77, "amp-decay"), (78, "amp-sustain")
    , (79, "amp-release")
    ]

-- * parse sysex

parse_patch :: ByteString -> Either String [Instrument.Patch]
parse_patch bytes = do
    bytes <- Sysex.expect_bytes bytes $
        korg_header <> ByteString.pack [0x40, 0x01]
    (record, _) <- Sysex.decode patch_spec $ dekorgify bytes
    (:[]) <$> extract_patch record

parse_patch_dump :: ByteString -> Either String [Instrument.Patch]
parse_patch_dump bytes = do
    bytes <- Sysex.expect_bytes bytes $ korg_header <> ByteString.pack [0x4c]
    -- Drop bank, program number, and 0 byte.
    patches $ dekorgify $ ByteString.drop 3 bytes
    where
    patches bytes
        | ByteString.length bytes >= Sysex.spec_bytes patch_spec = do
            (record, bytes) <- Sysex.decode patch_spec bytes
            patch <- extract_patch record
            patches <- patches bytes
            return (patch : patches)
        | otherwise = return []

extract_patch :: Sysex.Record -> Either String Instrument.Patch
extract_patch record = do
    name <- lookup "name"
    category <- lookup "category"
    pb_range <- (,) <$> lookup "pitch bend.intensity -"
        <*> lookup "pitch bend.intensity +"
    osc1 <- lookup "osc.0.type"
    osc2 <- lookup "osc.1.type"
    return $ (Instrument.patch (Instrument.instrument name [] pb_range))
        { Instrument.patch_tags =
            [("category", category), ("z1-osc", osc1), ("z1-osc", osc2)]
        }
    where
    lookup :: (Sysex.RecordVal a) => String -> Either String a
    lookup = flip Sysex.lookup_record record

-- | I think 0x30 should be 0x3g where g is the global channel, but I use 0
-- so this works.
korg_header :: ByteString.ByteString
korg_header = ByteString.pack [0xf0, Midi.korg_code, 0x30, 0x46]

-- | Z1 sysexes use a scheme where the eighth bits are packed into a single
-- byte preceeding its 7 7bit bytes.
dekorgify :: ByteString.ByteString -> ByteString.ByteString
dekorgify = mconcat . map smoosh . chunks
    where
    smoosh bs = case ByteString.uncons bs of
        Just (b7, bytes) -> snd $
            ByteString.mapAccumL (\i to -> (i+1, copy_bit b7 i to)) 0 bytes
        Nothing -> mempty
    copy_bit from i to = if Bits.testBit from i
        then Bits.setBit to 7 else Bits.clearBit to 7
    chunks bs
        | ByteString.length bs < 8 = [bs]
        | otherwise =
            let (pre, post) = ByteString.splitAt 8 bs
            in pre : chunks post

-- * specs

patch_spec :: [Spec]
patch_spec = Sysex.assert_valid "patch_spec"
    [ Str "name" 16
    , enum_byte "category" categories
    , byte "user group" 15
    , Bits
        [ ("hold", bits 1)
        , ("key priority", enum_bits 2 ["last", "low", "high"])
        , ("voice assign mode", enum_bits 5
            ["mono multi", "mono single", "poly"])
        ]
    , enum_byte "retrigger controllor" mod_source_list_2
    , ranged_byte "retrigger control threshold" (1, 127)
    , Bits
        [ ("unison type", enum_bits 2 ["off", "2", "3", "6"])
        , ("unison sw", boolean1)
        , ("unison mode", enum_bits 5 ["fixed", "dynamic"])
        ]
    , byte "unison detune" 99
    -- scale
    , Bits
        [ ("scale key", ranged_bits 4 (0, 11))
        , ("scale type", enum_bits 4 scale_types)
        ]
    , byte "random pitch intensity" 99
    , List "eg" 4 [Unparsed "unparsed" 19]
    , List "lfo" 4 [Unparsed "unparsed" 11]
    -- osc common
    , SubSpec "pitch bend"
        [ ranged_byte "intensity +" (-60, 24)
        , ranged_byte "intensity -" (-60, 24)
        , Bits $ let vals = ["0", "1/8", "1/4", "1/2"] ++ map show [1..12] in
            [ ("step +", enum_bits 4 vals)
            , ("step -", enum_bits 4 vals)
            ]
        ]
    , enum_byte "common pitch mod source" mod_source_list_1
    , modulation_intensity "common pitch mod intensity"
    , SubSpec "portamento"
        [ Bits
            [ ("sw", boolean1)
            , ("mode", enum_bits 7 ["normal", "fingered"])
            ]
        , byte "time" 99
        , enum_byte "mod source" mod_source_list_1
        , modulation_intensity "mod intensity"
        ]
    , List "osc" 2
        [ enum_byte "type" osc_types
        , enum_byte "octave" ["-2", "-1", "0", "1"] -- 32', 16', 8', 4'
        , ranged_byte "semi tone" (-12, 12)
        , ranged_byte "fine tune" (-50, 50)
        , ranged_byte "frequency offset" (-100, 100)
        -- pitch slope
        , byte "center key" 127
        , ranged_byte "lower slope" (-50, 100)
        , ranged_byte "higher slope" (-50, 100)
        -- pitch modulation
        , enum_byte "mod1 source" mod_source_list_1
        , modulation_intensity "mod1 intensity"
        , enum_byte "mod1 intensity controller" mod_source_list_1
        , modulation_intensity "mod1 intensity controller intensity"
        , enum_byte "mod2 source" mod_source_list_1
        , modulation_intensity "mod2 intensity"
        , Unparsed "setting" 38 -- union of osc params
        ]
    , Unparsed "sub osc" 14
    , Unparsed "noise generator" 8
    , Unparsed "mixer" 31
    , Unparsed "filter" 1
    , List "filter" 2 [Unparsed "unparsed" 27]
    , List "amp" 2 [Unparsed "unparsed" 9]
    , Unparsed "amp eg" 19
    , Unparsed "output" 4
    , Unparsed "effect" 72
    , Unparsed "controller" 4
    , Unparsed "link arpeggio" 13
    , List "pe knob" 5 [Unparsed "unparsed" 16]
    ]

osc_types :: [String]
osc_types =
    [ "standard", "comb", "vpm", "resonance", "ring-mod", "cross-mod", "sync"
    , "organ", "electric-piano", "brass", "reed", "plucked", "bowed"
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

modulation_intensity :: String -> Spec
modulation_intensity name = ranged_byte name (-99, 99)

-- * parse / generate sysex

test_multiset = do
    bytes <- ByteString.drop 9 <$> ByteString.readFile "inst_db/multi1.syx"
    return $ Sysex.decode multiset_spec (dekorgify bytes)

test_dump = do
    bytes <- ByteString.readFile "inst_db/z1_patches_b.syx"
    return $ parse_patch_dump bytes

test_patch = do
    bytes <- ByteString.readFile
        "inst_db/z1_sysex/z1 o00o00 ANALOG INIT.syx"
    return $ parse_patch bytes

read_patch = do
    b <- dekorgify . ByteString.drop 6 <$> ByteString.readFile
        "inst_db/z1_sysex/z1 o00o00 ANALOG INIT.syx"
    return $ Sysex.decode patch_spec b

-- | Spec to both parse and generate a multiset dump.
multiset_spec :: [Spec]
multiset_spec = Sysex.assert_valid "multiset_spec"
    [ Str "name" 16
    , List "timbre" 6 timbre
    , enum_byte "effect 1 select" effect_type1
    , Unparsed "effect 1 setting" 22
    -- , Union "effect 1 setting" "effect 1 select" 22 $ zip effect_type1
    --     [ effect_overdrive, effect_compressor, effect_parametric_eq
    --     , [], [], [], [], [], [], [], []
    --     ]
    , enum_byte "effect 2 select" effect_type2
    , Unparsed "effect 2 setting" 22
    -- , Union "effect 2 setting" "effect 2 select" 22 $ zip effect_type2 []
    , enum_byte "master effect select" master_effect_type
    , Unparsed "master effect setting" 18
    -- , Union "master effect setting" "master effect select" 18 $
    --     zip master_effect_type
    --     []
    ]

timbre :: [Spec]
timbre =
    -- timbre
    [ Bits
        [ ("program number", bits 7)
        , ("program bank", bits 1)
        ]
    , byte "reserve voice" 12
    -- pitch
    , ranged_byte "transpose" (-24, 24)
    , ranged_byte "detune" (-50, 50)
    , Bits
        [ ("arpeggiator", (4, boolean))
        , ("scale select", enum_bits 4 ["common", "program"])
        ]
    -- mixer
    , byte "output level" 128 -- 128 is PROG
    , byte "panpot" 128 -- 128 is PROG
    , byte "effect send" 101 -- 101 is PROG
    -- zone
    , byte "key zone top" 127
    , byte "key zone bottom" 127
    , ranged_byte "velocity zone top" (1, 127)
    , ranged_byte "velocity zone bottom" (1, 127)
    -- MIDI
    , Bits
        [ ("midi channel", ranged_bits 7 (0, 16)) -- 16 is GLOBAL
        , ("program change", boolean1)
        ]
    , Bits
        [ ("realtime edit", boolean1)
        , ("portamento sw", boolean1)
        , ("damper", boolean1)
        , ("x-y pad controller", boolean1)
        , ("modulation wheel", boolean1)
        , ("after touch", boolean1)
        , ("pitch bend", enum_bits 2 ["off", "common", "program"])
        ]
    , Bits
        [ ("", bits 6)
        , ("performance edit", boolean1)
        , ("others", boolean1)
        ]
    ] ++ reserved_space 1

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

effect_setting :: [Spec]
effect_setting = reserved_space 22

effect_overdrive :: [Spec]
effect_overdrive =
    [ enum_byte "mode" ["overdrive", "distortion"]
    , byte "drive" 99
    , byte "output level" 99
    , byte "pre low cutoff" 99
    , eq "low", eq "mid low", eq "mid high", eq "high"
    , effect_balance
    ]
    where eq prefix = SubSpec (prefix ++ " eq") eq_settings

effect_compressor :: [Spec]
effect_compressor =
    [ ranged_byte "sensitivity" (1, 99)
    , ranged_byte "attack" (1, 99)
    , byte "eq trim" 99
    , gain "pre low eq gain"
    , gain "pre high eq gain"
    , byte "output level" 99
    , effect_balance
    ]

effect_parametric_eq :: [Spec]
effect_parametric_eq =
    [ byte "trim" 99
    , SubSpec "low eq" $
        enum_byte "type" ["peaking", "shelving"] : eq_settings
    , SubSpec "mid low eq" $ eq_settings ++
        [ enum_byte "gain mod source" mod_source_list_2
        , gain "gain mod intensity"
        ]
    , SubSpec "mid high eq" eq_settings
    , SubSpec "high eq" $
        enum_byte "type" ["peaking", "shelving"] : eq_settings
    , effect_balance
    ]

eq_settings :: [Spec]
eq_settings = [byte "freq" 49, byte "q" 95, gain "gain"]

gain :: Sysex.Name -> Spec
gain name = ranged_byte name (-36, 36)

effect_balance :: Spec
effect_balance = SubSpec "effect balance"
    [ byte "balance" 100
    , enum_byte "effect balance mod source" mod_source_list_2
    , modulation_intensity "effect balance mod intensity"
    ]

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

master_effect_setting :: [Spec]
master_effect_setting = reserved_space 18
