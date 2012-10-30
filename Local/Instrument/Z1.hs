-- | Korg Z1 keyboard.
module Local.Instrument.Z1 where
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import Data.Word (Word8)

import System.FilePath ((</>))

import Util.Control
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
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
    patches <- (++) <$>
        Parse.patch_file (dir </> name)
        <*> Parse.parse_sysex_dir korg_sysex (dir </> "z1_sysex")
    MidiInst.save_patches synth patches name dir

synth :: Instrument.Synth
synth = Instrument.synth name synth_controls

synth_controls :: [(Midi.Control, String)]
synth_controls =
    -- The PE controls are the "performance expression" knobs whose effect
    -- depends on the instrument.
    [ (13, "pe1"), (20, "pe2"), (21, "pe3"), (22, "pe4"), (23, "pe5")
    , (16, "pad-x"), (17, "pad-y")
    , (65, "z1-port-sw") -- Turn portamento on and off.
    , (80, "z1-sw-1"), (81, "z1-sw-2") -- General purpose on/off switches.
    -- Various filter cutoff etc.
    ]

-- * parse sysex

-- TODO some of the dumps are program data dump format which starts
-- f0 42 30 46 - 4c <ub> <pp> 00
-- instead of
-- f0 42 30 46 - 40 01
--
-- I should write something to convert them into current program format.

-- Interactive testing.
-- tparse = Parse.parse_sysex_dir korg_sysex "Local/Instrument/z1_sysex"
-- tshow ps = mapM_ putStrLn (map Instrument.patch_summary ps)

korg_sysex :: Parse.ByteParser Instrument.Patch
korg_sysex = do
    Parse.start_sysex Midi.korg_code
    Parse.match_bytes [0x30, 0x46]
    patch <- current_program_dump
    Parse.end_sysex
    return patch

current_program_dump :: Parse.ByteParser Instrument.Patch
current_program_dump = do
    Parse.match_bytes [0x40, 0x01]
    contents <- fmap dekorgify Parse.to_eox
    return $ korg_patch contents

korg_patch :: [Word8] -> Instrument.Patch
korg_patch bytes = make_patch (name, category, (pb_up, pb_down), osc1, osc2)
    where
    -- These come from the sysex spec MIDIImp_z1 PROG_PRM.TXT from korg.
    common_off = 25 + 19*4 + 11*4
    osc1_off = common_off + 9
    osc2_off = osc1_off + 52
    -- The doc says [Intensity(+), Intensity(-)] but that seems to
    -- mean [down, up].
    [cat, u_pb_down, u_pb_up, osc1_type, osc2_type] =
        map (bytes!!) [16, common_off, common_off+1, osc1_off, osc2_off]
    pb_down = fromIntegral $ Sysex.to_signed 8 u_pb_down
    pb_up = fromIntegral $ Sysex.to_signed 8 u_pb_up
    name = Seq.strip $ Parse.to_string (take 16 bytes)
    category = Seq.at categories cat
    osc1 = Seq.at osc_types osc1_type
    osc2 = Seq.at osc_types osc2_type

make_patch ::
    (String, Maybe String, Control.PbRange, Maybe String, Maybe String)
    -> Instrument.Patch
make_patch (name, cat, pb_range, osc1, osc2) =
    -- Initialization will be filled in later.
    (Instrument.patch inst) { Instrument.patch_tags = tags }
    where
    inst = Instrument.instrument name [] pb_range
    tags = maybe_tags [("category", cat), ("z1-osc", osc1), ("z1-osc", osc2)]
    maybe_tags tags = [(k, v) | (k, Just v) <- tags]

-- | The Z1 has a built-in set of categories. Map the category index to the
-- name.
categories :: [String]
categories =
    [ "Synth-Hard", "Synth-Soft", "Synth-Lead", "Synth-Motion", "Synth-Bass"
    , "E.Piano", "Organ", "Keyboard", "Bell", "Strings", "Band/Choir"
    , "Brass", "Reed/Wind", "Guitar/Plucked", "Bass", "Percussive"
    , "Argpeggio", "SFX/Other"
    ]

osc_types :: [String]
osc_types =
    [ "standard", "comb", "vpm", "resonance", "ring-mod", "cross-mod", "sync"
    , "organ", "electric-piano", "brass", "reed", "plucked", "bowed"
    ]

-- | Z1 sysexes use a scheme where the eighth bits are packed into a single
-- byte preceeding its 7 7bit bytes.
dekorgify :: [Word8] -> [Word8]
dekorgify [] = []
dekorgify (b7:bytes) =
    [copy_bit b7 i b | (i, b) <- zip [0..] b7group] ++ dekorgify rest
    where
    (b7group, rest) = List.splitAt 7 bytes
    copy_bit from i to = if Bits.testBit from i
        then Bits.setBit to 7 else Bits.clearBit to 7


-- * parse / generate sysex

file = ByteString.readFile "multi1.syx"

-- F0,42,3g,46 header
-- 4d -- multi setup dump
-- 00
-- ub -- unit bank
-- mm -- multi num
-- 00
-- data
strip = dekorg . ByteString.drop 9
dekorg = ByteString.pack . dekorgify . ByteString.unpack

test = do
    b <- strip <$> file
    return $ Sysex.decode multiset b

rec = Sysex.spec_to_record multiset

-- | Spec to both parse and generate a multiset dump.
multiset :: [Spec]
multiset = Sysex.assert_valid "multiset" $
    [ Str "name" 16
    , List "timbre" 6 timbre
    , enum_byte "effect1 select" effect_type1
    , SubSpec "effect1 setting" effect_setting
    , enum_byte "effect2 select" effect_type2
    , SubSpec "effect2 setting" effect_setting
    , enum_byte "master effect select" master_effect_type
    , SubSpec "master effect setting" master_effect_setting
    ]

timbre :: [Spec]
timbre =
    -- timbre
    [ Bits
        [ ("program bank", bits 1)
        , ("program number", bits 7)
        ]
    , byte "reserve voice" 12
    -- pitch
    , ranged_byte "transpose" (-24, 24)
    , ranged_byte "detune" (-50, 50)
    , Bits
        [ ("scale select", enum_bits 4 ["common", "program"])
        , ("arpeggiator", (4, boolean))
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
        [ ("program change", boolean1)
        , ("midi channel", ranged_bits 7 (0, 16)) -- 16 is GLOBAL
        ]
    , Bits
        [ ("pitch bend", enum_bits 2 ["off", "common", "program"])
        , ("after touch", boolean1)
        , ("modulation wheel", boolean1)
        , ("x-y pad controller", boolean1)
        , ("damper", boolean1)
        , ("portamento sw", boolean1)
        , ("realtime edit", boolean1)
        ]
    , Bits
        [ ("performance edit", boolean1)
        , ("others", boolean1)
        , ("", bits 6)
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

effect_4_band_peq :: [Spec]
effect_4_band_peq =
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
    , ranged_byte "effect balance mod intensity" (-99, 99)
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
