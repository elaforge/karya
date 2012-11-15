{-# LANGUAGE ScopedTypeVariables #-}
-- | Korg Z1 keyboard.
module Local.Instrument.Z1 where
import qualified Data.Bits as Bits
import Data.Bits ((.|.))
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import Data.Word (Word8)

import System.FilePath ((</>))

import Util.Control
import Util.Pretty (pprint)
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Sysex as Sysex
import Instrument.Sysex
       (Specs, Spec(..), bits, ranged_bits, unsigned, bool_bit, enum,
        enum_bits, ranged)
import qualified App.MidiInst as MidiInst


synth_name :: FilePath
synth_name = "z1"

load :: FilePath -> IO [MidiInst.SynthDesc]
load = MidiInst.load_db (const MidiInst.empty_code) synth_name

make_db :: FilePath -> IO ()
make_db dir = do
    bank_a <- Sysex.parse_builtins 0 program_dump
        (dir </> synth_name </> "bank_a.syx")
    bank_b <- Sysex.parse_builtins 1 program_dump
        (dir </> synth_name </> "bank_b.syx")
    sysex <- Sysex.parse_dir [current_program_dump, program_dump, sysex_manager]
        (dir </> synth_name </> "sysex")
    MidiInst.save_patches synth (concat [bank_a, bank_b, sysex]) synth_name dir
    where
    current_program_dump =
        fmap (:[]) . (rmap_to_patch <=< decode_current_program)
    program_dump = mapM rmap_to_patch <=< decode_program_dump
    sysex_manager = mapM rmap_to_patch <=< decode_sysex_manager

synth :: Instrument.Synth
synth = Instrument.synth synth_name synth_controls

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

-- * decode sysex

decode_current_program :: ByteString -> Either String Sysex.RMap
decode_current_program bytes = do
    (header, bytes) <- decode current_program_dump_header bytes
    (rmap, _) <- decode patch_spec (dekorg bytes)
    return $ header <> rmap

decode_program_dump :: ByteString -> Either String [Sysex.RMap]
decode_program_dump bytes = do
    -- If there is just one, then the bank and unit fields are valid.
    -- Otherwise, they are 0.
    (rmap, bytes) <- decode program_dump_header bytes
    let syxs = exact_chunks (Sysex.spec_bytes config patch_spec) (dekorg bytes)
    mapM (fmap ((rmap <>) . fst) . decode patch_spec) syxs

decode_sysex_manager :: ByteString -> Either String [Sysex.RMap]
decode_sysex_manager bytes = do
    bytes <- Sysex.expect_bytes bytes $ Char8.pack "Sysex Manager"
    let sysexes = Sysex.extract_sysex bytes
    -- The first sysex is something else.
    mapM decode_current_program (drop 1 sysexes)

test_decode = do
    -- let fn = "inst_db/z1/sysex/lib2/apollo44.syx"
    -- let fn = "inst_db/z1/sysex/lib1/z1 o00o00 Syncapacitor.syx"
    -- let fn = "inst_db/z1/sysex/lib1/z1 o00o05 Composite Synth.syx"
    let fn = "inst_db/z1/sysex/lib1/z1 o00o00 .C.H.A.O.S..syx"
    decode_current_program <$> B.readFile fn

-- * encode sysex

-- set_pitch_bend :: FilePath -> IO ()
-- set_pitch_bend fn = do
--     bytes <- B.readFile fn
--     records <- require "parse" $ decode_program_dump bytes
--     records <- require "set" $ mapM set records
--     bytes <- require "unparse" $ unparse_patch_dump records
--     B.writeFile (fn ++ ".modified") bytes
--     where
--     set = Sysex.put_rmap "pitch bend.intensity +" (24 :: Int)
--         <=< Sysex.put_rmap "pitch bend.intensity -" (-24 :: Int)
--     require msg = either (errorIO . ((msg ++ ": ") ++)) return

-- encode_rmap :: Sysex.RMap -> Either String ByteString
-- encode_rmap rmap = do
--     let has_bank = case Sysex.lookup_rmap "bank" rmap of
--             Right (s :: String) -> True
--             Left _ -> False
--     if has_bank then encode_current_program rmap else encode_program_dump rmap

encode_current_program :: Sysex.RMap -> Either String ByteString
encode_current_program rmap =
    encode_sysex (encode current_program_dump_header rmap)
        (encode patch_spec rmap)

encode_program_dump :: Sysex.RMap -> Either String ByteString
encode_program_dump rmap =
    encode_sysex (encode program_dump_header rmap)
        (encode patch_spec rmap)

data Unit = Program | Bank | All deriving (Show)
data Bank = A | B deriving (Show)

encode_bank_dump :: Unit -> Bank -> [Sysex.RMap] -> Either String ByteString
encode_bank_dump unit bank rmaps = do
    header_rmap <- set_bank $ Sysex.spec_to_rmap program_dump_header
    encode_sysex (encode program_dump_header header_rmap)
        (mconcat <$> mapM (encode patch_spec) rmaps)
    where
    set_bank = Sysex.put_rmap "bank" (map Char.toLower (show bank))
        <=< Sysex.put_rmap "unit" (map Char.toLower (show unit))

encode_sysex :: Either String ByteString -> Either String ByteString
    -> Either String ByteString
encode_sysex encode_header encode_body = do
    header <- encode_header
    body <- encode_body
    return $ header <> enkorg body <> B.singleton Midi.eox_byte

-- ** record

rmap_to_patch :: Sysex.RMap -> Either String Instrument.Patch
rmap_to_patch rmap = do
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
    lookup = flip Sysex.lookup_rmap rmap

current_multi_data_dump :: Word8
current_multi_data_dump = 0x69

multi_data_dump :: Word8
multi_data_dump = 0x4d

-- | Z1 sysexes use a scheme where the eighth bits are packed into a single
-- byte preceeding its 7 7bit bytes.
dekorg :: ByteString -> ByteString
dekorg = mconcat . map smoosh . chunks 8
    where
    smoosh bs = case B.uncons bs of
        Just (b7, bytes) -> snd $
            B.mapAccumL (\i to -> (i+1, copy_bit b7 i to)) 0 bytes
        Nothing -> mempty
    copy_bit from i to = if Bits.testBit from i
        then Bits.setBit to 7 else Bits.clearBit to 7

enkorg :: ByteString -> ByteString
enkorg = mconcat . map expand . chunks 7
    where
    expand bs = B.cons bits (B.map (`Bits.clearBit` 7) bs)
        where bits = B.foldr get_bits 0 bs
    get_bits b accum =
        Bits.shiftL accum 1 .|. (if Bits.testBit b 7 then 1 else 0)

chunks :: Int -> ByteString -> [ByteString]
chunks size bs
    | B.null pre = []
    | otherwise = pre : chunks size post
    where (pre, post) = B.splitAt size bs

exact_chunks :: Int -> ByteString -> [ByteString]
exact_chunks size bs
    | B.length pre < size = []
    | otherwise = pre : exact_chunks size post
    where (pre, post) = B.splitAt size bs

-- * specs

config :: Sysex.Config
config = Sysex.config_8bit

encode :: Specs -> Sysex.RMap -> Either String ByteString
encode = Sysex.encode config

decode :: Specs -> ByteString -> Either String (Sysex.RMap, ByteString)
decode = Sysex.decode config

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

patch_spec :: Specs
patch_spec = Sysex.assert_valid "patch_spec"
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
    , ("eg", List 4 [("unparsed", Unparsed 19)])
    , ("lfo", List 4 [("unparsed", Unparsed 11)])
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
    , ("common pitch mod intensity", modulation_intensity)
    , ("portamento", SubSpec
        [ ("", Bits
            [ ("sw", bool_bit)
            , ("mode", enum_bits 7 ["normal", "fingered"])
            ])
        , ("time", unsigned 99)
        , ("mod source", enum mod_source_list_1)
        , ("mod intensity", modulation_intensity)
        ])
    , ("osc", List 2
        [ ("type", enum osc_types)
        , ("octave", enum ["-2", "-1", "0", "1"]) -- 32', 16', 8', 4'
        , ("semi tone", ranged (-12) 12)
        , ("fine tune", ranged (-50) 50)
        , ("frequency offset", ranged (-100) 100)
        -- pitch slope
        , ("center key", unsigned 127)
        , ("lower slope", ranged (-50) 100)
        , ("higher slope", ranged (-50) 100)
        -- pitch modulation
        , ("mod1 source", enum mod_source_list_1)
        , ("mod1 intensity", modulation_intensity)
        , ("mod1 intensity controller", enum mod_source_list_1)
        , ("mod1 intensity controller intensity", modulation_intensity)
        , ("mod2 source", enum mod_source_list_1)
        , ("mod2 intensity", modulation_intensity)
        , ("setting", Unparsed 38) -- union of osc params
        ])
    , ("sub osc", Unparsed 14)
    , ("noise generator", Unparsed 8)
    , ("mixer", Unparsed 31)
    , ("filter setting", Unparsed 1)
    , ("filter", List 2 [("unparsed", Unparsed 27)])
    , ("amp", List 2 [("unparsed", Unparsed 9)])
    , ("amp eg", Unparsed 19)
    , ("output", Unparsed 4)
    , ("effect", Unparsed 72)
    , ("controller", Unparsed 4)
    , ("link arpeggio", Unparsed 13)
    , ("pe knob", List 5 [("unparsed", Unparsed 16)])
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

modulation_intensity :: Spec
modulation_intensity = ranged (-99) 99

-- * parse / generate sysex

test_multiset = do
    bytes <- B.drop 9 <$> B.readFile "inst_db/multi1.syx"
    return $ decode multiset_spec (dekorg bytes)

test_dump = do
    bytes <- B.readFile "inst_db/z1/bank_b.syx"
    return $ decode_program_dump bytes

test_encode = do
    bytes <- B.readFile "inst_db/z1/bank_b.syx"
    let Right recs = decode_program_dump bytes
    return $ encode patch_spec (head recs)

test_patch = do
    bytes <- B.readFile
        "inst_db/z1/sysex/lib1/z1 o00o00 ANALOG INIT.syx"
    return $ decode_current_program bytes

read_patch = do
    b <- dekorg . B.drop 6 <$> B.readFile
        "inst_db/z1/sysex/lib1/z1 o00o00 ANALOG INIT.syx"
    return $ decode patch_spec b

-- | Spec to both parse and generate a multiset dump.
multiset_spec :: Specs
multiset_spec = Sysex.assert_valid "multiset_spec"
    [ ("name", Str 16)
    , ("timbre", List 6 timbre)
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
    ]

timbre :: Specs
timbre =
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
eq_settings = [("freq", unsigned 49), ("q", unsigned 95), ("gain", gain)]

gain :: Spec
gain = ranged (-36) 36

effect_balance :: (Sysex.Name, Spec)
effect_balance = ("effect balance", SubSpec
    [ ("balance", unsigned 100)
    , ("effect balance mod source", enum mod_source_list_2)
    , ("effect balance mod intensity", modulation_intensity)
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
