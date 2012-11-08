-- | Korg Z1 keyboard.
module Local.Instrument.Z1 where
import qualified Data.Bits as Bits
import Data.Bits ((.|.))
import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import System.FilePath ((</>))

import Util.Control
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Sysex as Sysex
import Instrument.Sysex
       (Spec(..), bits, ranged_bits, unsigned, bool_bit, enum, enum_bits,
        ranged)
import qualified App.MidiInst as MidiInst


name :: FilePath
name = "z1"

load :: FilePath -> IO [MidiInst.SynthDesc]
load = MidiInst.load_db (const MidiInst.empty_code) name

make_db :: FilePath -> IO ()
make_db dir = do
    bank_a <- Sysex.parse_bank_dump 0 patch_dump
        (dir </> name </> "bank_a.syx")
    bank_b <- Sysex.parse_bank_dump 1 patch_dump
        (dir </> name </> "bank_b.syx")
    sysex <- Sysex.parse_dir [patch, patch_dump]
        (dir </> name </> "sysex")
    MidiInst.save_patches synth (concat [bank_a, bank_b, sysex]) name dir
    where
    patch = fmap (:[]) . (record_to_patch <=< parse_patch)
    patch_dump = mapM record_to_patch <=< parse_patch_dump

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

parse_patch :: ByteString -> Either String Sysex.Record
parse_patch bytes = do
    bytes <- Sysex.expect_bytes bytes $
        korg_header <> B.pack [0x40, 0x01]
    fst <$> decode patch_spec (dekorg bytes)

unparse_patch :: Sysex.Record -> Either String ByteString
unparse_patch record = do
    body <- enkorg <$> encode patch_spec record
    return $ korg_header <> B.pack [0x40, 0x01] <> body

set_pitch_bend :: FilePath -> IO ()
set_pitch_bend fn = do
    bytes <- B.readFile fn
    records <- require "parse" $ parse_patch_dump bytes
    records <- require "set" $ mapM set records
    bytes <- require "unparse" $ unparse_patch_dump records
    B.writeFile (fn ++ ".modified") bytes
    where
    set = Sysex.put_record "pitch bend.intensity +" (24 :: Int)
        <=< Sysex.put_record "pitch bend.intensity -" (-24 :: Int)
    require msg = either (errorIO . ((msg ++ ": ") ++)) return

parse_patch_dump :: ByteString -> Either String [Sysex.Record]
parse_patch_dump bytes = do
    bytes <- Sysex.expect_bytes bytes $ korg_header <> B.pack [0x4c]
    -- Drop bank, program number, and 0 byte.
    patches $ dekorg $ B.drop 3 bytes
    where
    patches bytes
        | B.length bytes >= Sysex.spec_bytes parse_config patch_spec = do
            (record, bytes) <- decode patch_spec bytes
            records <- patches bytes
            return (record : records)
        | otherwise = return []

unparse_patch_dump :: [Sysex.Record] -> Either String ByteString
unparse_patch_dump records = do
    body <- enkorg . mconcat <$> mapM (encode patch_spec) records
    -- 0s for bank, program number, and padding.
    return $ korg_header <> B.pack [0x4c, 0, 0, 0] <> body

record_to_patch :: Sysex.Record -> Either String Instrument.Patch
record_to_patch record = do
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
korg_header :: ByteString
korg_header = B.pack [0xf0, Midi.korg_code, 0x30, 0x46]

-- | Z1 sysexes use a scheme where the eighth bits are packed into a single
-- byte preceeding its 7 7bit bytes.
dekorg :: ByteString -> ByteString
dekorg = mconcat . map smoosh . chunked 8
    where
    smoosh bs = case B.uncons bs of
        Just (b7, bytes) -> snd $
            B.mapAccumL (\i to -> (i+1, copy_bit b7 i to)) 0 bytes
        Nothing -> mempty
    copy_bit from i to = if Bits.testBit from i
        then Bits.setBit to 7 else Bits.clearBit to 7

enkorg :: ByteString -> ByteString
enkorg = mconcat . map expand . chunked 7
    where
    expand bs = B.cons bits (B.map (`Bits.clearBit` 7) bs)
        where bits = B.foldl' get_bits 0 bs
    get_bits accum b =
        Bits.shiftL accum 1 .|.  (if Bits.testBit b 7 then 1 else 0)

chunked :: Int -> ByteString -> [ByteString]
chunked size bs
    | B.length bs <= size = [bs]
    | otherwise =
        let (pre, post) = B.splitAt size bs
        in pre : chunked size post

-- * specs

encode :: [Spec] -> Sysex.Record -> Either String ByteString
encode = Sysex.encode parse_config

decode :: [Spec] -> ByteString -> Either String (Sysex.Record, ByteString)
decode = Sysex.decode parse_config

parse_config :: Sysex.Config
parse_config = Sysex.Config decode_num encode_num (const 1)

-- | Z1 nums are always one 8bit byte.
decode_num :: Sysex.NumRange -> ByteString -> Int
decode_num (low, _) bytes = case B.uncons bytes of
    Nothing -> 0
    Just (b, _)
        | low < 0 -> Sysex.to_signed 8 b
        | otherwise -> fromIntegral b

encode_num :: Sysex.NumRange -> Int -> ByteString
encode_num (low, _) num
    | low < 0 = B.singleton $ Sysex.from_signed 8 num
    | otherwise = B.singleton (fromIntegral num)

patch_spec :: [Spec]
patch_spec = Sysex.assert_valid "patch_spec"
    [ Str "name" 16
    , enum "category" categories
    , unsigned "user group" 15
    , Bits
        [ ("hold", bits 1)
        , ("key priority", enum_bits 2 ["last", "low", "high"])
        , ("voice assign mode", enum_bits 5
            ["mono multi", "mono single", "poly"])
        ]
    , enum "retrigger controllor" mod_source_list_2
    , ranged "retrigger control threshold" 1 127
    , Bits
        [ ("unison type", enum_bits 2 ["off", "2", "3", "6"])
        , ("unison sw", bool_bit)
        , ("unison mode", enum_bits 5 ["fixed", "dynamic"])
        ]
    , unsigned "unison detune" 99
    -- scale
    , Bits
        [ ("scale key", ranged_bits 4 (0, 11))
        , ("scale type", enum_bits 4 scale_types)
        ]
    , unsigned "random pitch intensity" 99
    , List "eg" 4 [Unparsed "unparsed" 19]
    , List "lfo" 4 [Unparsed "unparsed" 11]
    -- osc common
    , SubSpec "pitch bend"
        [ ranged "intensity +" (-60) 24
        , ranged "intensity -" (-60) 24
        , Bits $ let vals = ["0", "1/8", "1/4", "1/2"] ++ map show [1..12] in
            [ ("step +", enum_bits 4 vals)
            , ("step -", enum_bits 4 vals)
            ]
        ]
    , enum "common pitch mod source" mod_source_list_1
    , modulation_intensity "common pitch mod intensity"
    , SubSpec "portamento"
        [ Bits
            [ ("sw", bool_bit)
            , ("mode", enum_bits 7 ["normal", "fingered"])
            ]
        , unsigned "time" 99
        , enum "mod source" mod_source_list_1
        , modulation_intensity "mod intensity"
        ]
    , List "osc" 2
        [ enum "type" osc_types
        , enum "octave" ["-2", "-1", "0", "1"] -- 32', 16', 8', 4'
        , ranged "semi tone" (-12) 12
        , ranged "fine tune" (-50) 50
        , ranged "frequency offset" (-100) 100
        -- pitch slope
        , unsigned "center key" 127
        , ranged "lower slope" (-50) 100
        , ranged "higher slope" (-50) 100
        -- pitch modulation
        , enum "mod1 source" mod_source_list_1
        , modulation_intensity "mod1 intensity"
        , enum "mod1 intensity controller" mod_source_list_1
        , modulation_intensity "mod1 intensity controller intensity"
        , enum "mod2 source" mod_source_list_1
        , modulation_intensity "mod2 intensity"
        , Unparsed "setting" 38 -- union of osc params
        ]
    , Unparsed "sub osc" 14
    , Unparsed "noise generator" 8
    , Unparsed "mixer" 31
    , Unparsed "filter setting" 1
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
modulation_intensity name = ranged name (-99) 99

-- * parse / generate sysex

test_multiset = do
    bytes <- B.drop 9 <$> B.readFile "inst_db/multi1.syx"
    return $ decode multiset_spec (dekorg bytes)

test_dump = do
    bytes <- B.readFile "inst_db/z1/bank_b.syx"
    return $ parse_patch_dump bytes

test_encode = do
    bytes <- B.readFile "inst_db/z1/bank_b.syx"
    let Right recs = parse_patch_dump bytes
    return $ encode patch_spec (head recs)

test_patch = do
    bytes <- B.readFile
        "inst_db/z1/sysex/lib1/z1 o00o00 ANALOG INIT.syx"
    return $ parse_patch bytes

read_patch = do
    b <- dekorg . B.drop 6 <$> B.readFile
        "inst_db/z1/sysex/lib1/z1 o00o00 ANALOG INIT.syx"
    return $ decode patch_spec b

-- | Spec to both parse and generate a multiset dump.
multiset_spec :: [Spec]
multiset_spec = Sysex.assert_valid "multiset_spec"
    [ Str "name" 16
    , List "timbre" 6 timbre
    , enum "effect 1 select" effect_type1
    , Unparsed "effect 1 setting" 22
    -- , Union "effect 1 setting" "effect 1 select" 22 $ zip effect_type1
    --     [ effect_overdrive, effect_compressor, effect_parametric_eq
    --     , [], [], [], [], [], [], [], []
    --     ]
    , enum "effect 2 select" effect_type2
    , Unparsed "effect 2 setting" 22
    -- , Union "effect 2 setting" "effect 2 select" 22 $ zip effect_type2 []
    , enum "master effect select" master_effect_type
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
    , unsigned "reserve voice" 12
    -- pitch
    , ranged "transpose" (-24) 24
    , ranged "detune" (-50) 50
    , Bits
        [ ("arpeggiator", (4, Sysex.Enum ["on", "off"]))
        , ("scale select", enum_bits 4 ["common", "program"])
        ]
    -- mixer
    , unsigned "output level" 128 -- 128 is PROG
    , unsigned "panpot" 128 -- 128 is PROG
    , unsigned "effect send" 101 -- 101 is PROG
    -- zone
    , unsigned "key zone top" 127
    , unsigned "key zone bottom" 127
    , ranged "velocity zone top" 1 127
    , ranged "velocity zone bottom" 1 127
    -- MIDI
    , Bits
        [ ("midi channel", ranged_bits 7 (0, 16)) -- 16 is GLOBAL
        , ("program change", bool_bit)
        ]
    , Bits
        [ ("realtime edit", bool_bit)
        , ("portamento sw", bool_bit)
        , ("damper", bool_bit)
        , ("x-y pad controller", bool_bit)
        , ("modulation wheel", bool_bit)
        , ("after touch", bool_bit)
        , ("pitch bend", enum_bits 2 ["off", "common", "program"])
        ]
    , Bits
        [ ("unused", bits 6)
        , ("performance edit", bool_bit)
        , ("others", bool_bit)
        ]
    , Unparsed "" 1
    ]

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

effect_overdrive :: [Spec]
effect_overdrive =
    [ enum "mode" ["overdrive", "distortion"]
    , unsigned "drive" 99
    , unsigned "output level" 99
    , unsigned "pre low cutoff" 99
    , eq "low", eq "mid low", eq "mid high", eq "high"
    , effect_balance
    ]
    where eq prefix = SubSpec (prefix ++ " eq") eq_settings

effect_compressor :: [Spec]
effect_compressor =
    [ ranged "sensitivity" 1 99
    , ranged "attack" 1 99
    , unsigned "eq trim" 99
    , gain "pre low eq gain"
    , gain "pre high eq gain"
    , unsigned "output level" 99
    , effect_balance
    ]

effect_parametric_eq :: [Spec]
effect_parametric_eq =
    [ unsigned "trim" 99
    , SubSpec "low eq" $
        enum "type" ["peaking", "shelving"] : eq_settings
    , SubSpec "mid low eq" $ eq_settings ++
        [ enum "gain mod source" mod_source_list_2
        , gain "gain mod intensity"
        ]
    , SubSpec "mid high eq" eq_settings
    , SubSpec "high eq" $
        enum "type" ["peaking", "shelving"] : eq_settings
    , effect_balance
    ]

eq_settings :: [Spec]
eq_settings = [unsigned "freq" 49, unsigned "q" 95, gain "gain"]

gain :: Sysex.Name -> Spec
gain name = ranged name (-36) 36

effect_balance :: Spec
effect_balance = SubSpec "effect balance"
    [ unsigned "balance" 100
    , enum "effect balance mod source" mod_source_list_2
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
