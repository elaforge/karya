{-# LANGUAGE ScopedTypeVariables #-}
-- | Yamaha VL1 synthesizer.
module Local.Instrument.Vl1m where
import qualified Data.Bits as Bits
import Data.Bits ((.|.), (.&.))
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Word (Word8)

import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import Util.Control
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Num as Num
import Util.Pretty (pprint)
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Midi.Parse
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Sysex as Sysex
import Instrument.Sysex
       (Specs, Spec(..), unsigned, bool, enum, ranged, signed)
import qualified App.MidiInst as MidiInst


synth_name :: String
synth_name = "vl1"

load :: FilePath -> IO [MidiInst.SynthDesc]
load = MidiInst.load_db (const MidiInst.empty_code) synth_name

builtin :: FilePath
builtin = "vl1v2-factory/vl1_ver2.all"

-- | Read the patch file, scan the sysex dir, and save the results in a cache.
make_db :: FilePath -> IO ()
make_db dir = do
    vc_patches <- parse_dir (dir </> synth_name </> "vc")
    sysex_patches <- parse_dir (dir </> synth_name </> "sysex")
    builtins <- parse_builtins (dir </> synth_name </> builtin)
    MidiInst.save_patches synth (builtins ++ vc_patches ++ sysex_patches)
        synth_name dir

synth :: Instrument.Synth
synth = Instrument.synth synth_name []

-- * parse

test_check = do
    bs <- fix_sysex <$> B.readFile "tmp/vl0.syx"
    print (Num.hex (checksum bs))
    -- fs <- File.list_dir "flugel"
    -- forM_ fs $ \f -> do
    --     bs <- fix_sysex <$> B.readFile f
    --     print (f, checksum bs, Num.hex $ B.index bs (B.length bs - 2))

test_split = do
    bytes <- B.readFile $ "inst_db" </> synth_name </> builtin
    let syxs = split_1bk (Just 0) bytes
    forM_ (zip [0..] (take 1 syxs)) $ \(n, syx) ->
        B.writeFile ("tmp/vl" ++ show n ++ ".syx") (unfix_sysex syx)

test_record = do
    -- let fn = "./inst_db/vl1/sysex/krikke/babyphon/septictank(vl1).syx"
    -- let fn = "tmp/vl0.syx"
    -- let fn = "record-sysex0.syx"
    -- let fn = "inst_db/vl1/" ++ builtin
    let fn = "patchman1.syx"
    syxs <- file_to_syx fn
    return $ map parse_sysex syxs

dump_instruments :: FilePath -> IO ()
dump_instruments fn = do
    results <- parse_file fn
    let name_of = Instrument.inst_name . Instrument.patch_instrument
    pprint $ map (fmap name_of) results

-- ***  Local/Instrument/Vl1m.hs:91 [parse_dir] - parsing "./inst_db/vl1/sysex/krikke/babyphon/septictank(vl1).syx"/1: both elements off: SepticTank
-- ***  Local/Instrument/Vl1m.hs:91 [parse_dir] - parsing "./inst_db/vl1/sysex/krikke/babyphon/septictank(vl1).syx"/2: too few bytes
-- From:   demandInput

test_patch = do
    Right r : _ <- test_record
    return $ record_to_patch r

parse_builtins :: FilePath -> IO [Instrument.Patch]
parse_builtins fn = do
    results <- parse_file fn
    mapM_ Log.warn (Either.lefts results)
    return [Sysex.initialize_program 0 i patch
        | (i, Right patch) <- zip [0..] results]

parse_dir :: FilePath -> IO [Instrument.Patch]
parse_dir dir = do
    fns <- File.recursive_list_dir (const True) dir
    (warns, patches) <- Seq.partition_either . concat <$> mapM parse_file fns
    mapM_ Log.warn warns
    return patches

parse_file :: FilePath -> IO [Either String Instrument.Patch]
parse_file fn = do
    syxs <- file_to_syx fn
    txt <- fromMaybe "" <$>
        File.ignore_enoent (readFile (FilePath.replaceExtension fn ".txt"))
    let results = map (record_to_patch <=< parse_sysex) syxs
    return [either (Left . failed i) (Right . combine fn txt syx) result
        | (i, syx, result) <- zip3 [1..] syxs results]
    where
    failed i msg = "parsing " ++ show fn ++ "/" ++ show i ++ ": " ++ msg

combine :: FilePath -> String -> ByteString -> Instrument.Patch
    -> Instrument.Patch
combine fn txt syx patch = Sysex.add_file fn $ patch
    { Instrument.patch_text = Seq.strip txt
    , Instrument.patch_initialize =
        Instrument.InitializeMidi [Midi.Parse.decode syx]
    }

parse_sysex :: ByteString -> Either String Sysex.RMap
parse_sysex bytes = fst <$> decode patch_spec bytes

file_to_syx :: FilePath -> IO [ByteString]
file_to_syx fn = map fix_sysex <$> case FilePath.takeExtension fn of
        ".all" -> split_1bk Nothing <$> B.readFile fn
        ".1vc" -> split_1vc <$> B.readFile fn
        ".1bk" -> split_1bk Nothing <$> B.readFile fn
        ".syx" -> split_syx <$> B.readFile fn
        ".txt" -> return []
        _ -> Log.warn ("skipping " ++ show fn) >> return []
    where
    -- | Convert .1vc format to .syx format.  Derived by looking at vlone70
    -- conversions with od.
    split_1vc bytes = [bytes_to_syx Nothing (B.drop 0xc00 bytes)]
    split_syx = map (<> B.singleton Midi.eox_byte) . filter (not . B.null)
        . B.split Midi.eox_byte

split_1bk :: Maybe Word8 -> ByteString -> [ByteString]
split_1bk memory =
    zipWith (\n -> bytes_to_syx ((+n) <$> memory)) [0..] . split
    where
    split bytes = takeWhile (not . B.all (==0) . B.take 20) $
        map (flip B.drop bytes) offsets
    offsets = [0xc00, 0x1800..]

vl1_header :: Int -> ByteString
vl1_header nbytes = B.pack
    -- spec sheet says there's a 'device number' after yamaha_code
    [ 0xf0, Midi.yamaha_code, 0, 0x7a
    , msb, lsb -- size counts from 'L' char until the checksum
    ] <> magic
    where
    (lsb, msb) = Midi.split14 (nbytes + B.length magic)
    magic = Char8.pack "LM 20117VC"

-- | For some reason, some sysexes come out with a 0 for device numbers, and
-- some omit it entirely.
fix_sysex :: ByteString -> ByteString
fix_sysex bytes
    | B.isPrefixOf short bytes = long <> B.drop (B.length short) bytes
    | otherwise = bytes
    where
    long = B.pack [0xf0, Midi.yamaha_code, 0, 0x7a]
    short = B.pack [0xf0, Midi.yamaha_code, 0x7a]

unfix_sysex bytes
    | B.isPrefixOf long bytes = short <> B.drop 4 bytes
    | otherwise = bytes
    where
    long = B.pack [0xf0, Midi.yamaha_code, 0, 0x7a]
    short = B.pack [0xf0, Midi.yamaha_code, 0x7a]

-- | Wrap sysex codes around the raw bytes.
bytes_to_syx :: Maybe Word8 -> ByteString -> ByteString
bytes_to_syx memory bytes = append_suffix $
    vl1_header (2 + 14 + size)
        -- memory type, memory number
        <> B.pack (maybe [0x7f, 0] (\n -> [0, n]) memory)
        <> B.replicate 14 0 -- padding
        <> B.take size bytes
    where size = 0xc1c - 0x20

append_suffix :: ByteString -> ByteString
append_suffix bytes = bytes <> B.pack [checksum, Midi.eox_byte]
    where
    -- Checksum is the 2s complement of 7bit sum of the data.
    checksum = (2^7 - val) .&. 0x7f
    -- Drop vl1_header but keep the magic string.
    val = B.foldl (+) 0 (B.drop 6 bytes)

checksum :: ByteString -> Word8
checksum bytes = (2^7 - val) .&. 0x7f
    where
    suf = B.drop 6 bytes
    bs = B.take (B.length suf - 2) suf
    val = B.foldl (+) 0 bs

-- * record

-- | Each voice has two elements, each with their own PbRange, name, and
-- controls.
type ElementInfo = (Control.PbRange, String, [(Midi.Control, [String])])

record_to_patch :: Sysex.RMap -> Either String Instrument.Patch
record_to_patch rmap = do
    name <- lookup "name"
    elt1 <- extract_element 0 rmap
    maybe_elt2 <- ifM ((=="dual") <$> lookup "voice mode")
        (Just <$> extract_element 1 rmap)
        (return Nothing)
    vl1_patch name elt1 maybe_elt2
    where
    lookup :: (Sysex.RecordVal a) => String -> Either String a
    lookup = flip Sysex.lookup_rmap rmap

vl1_patch :: Instrument.InstrumentName -> ElementInfo
    -> Maybe ElementInfo -> Either String Instrument.Patch
vl1_patch name elt1 maybe_elt2 =
    return $ (Instrument.patch inst)
        { Instrument.patch_tags = map ((,) "vl1-element") names }
    where
    inst = Instrument.instrument name cmap pb_range
    (pb_ranges, names, cc_groups) =
        unzip3 $ elt1 : Maybe.maybeToList maybe_elt2

    -- Optimistically take the widest range.
    Just pb_range = Seq.maximum_on (\(low, high) -> max (abs low) (abs high))
        pb_ranges
    cmap = Map.assocs $ Map.mapMaybe highest_prio $
        Map.unionsWith (++) (map Map.fromList cc_groups)
    highest_prio cs = List.find (`elem` cs) (map fst vl1_control_map)

extract_element :: Int -> Sysex.RMap -> Either String ElementInfo
extract_element n rmap = do
    controls <- forM vl1_control_map $ \(name, has_upper_lower) -> do
        cc <- lookup [name, "control"]
        depths <- if has_upper_lower
            then do
                upper <- lookup [name, "upper depth"]
                lower <- lookup [name, "lower depth"]
                return [upper, lower]
            else (:[]) <$> lookup [name, "depth"]
        return (name, cc, depths)
    pb_up <- lookup ["pitch", "upper depth"]
    pb_down <- lookup ["pitch", "lower depth"]
    name <- lookup ["name"]
    return ((pb_up, pb_down), name, process_controls controls)
    where
    lookup :: (Sysex.RecordVal a) => [String] -> Either String a
    lookup k = Sysex.lookup_rmap (Seq.join "." (["element", show n] ++ k)) rmap
    -- The vl1 mostly uses the midi control list, except sticks some
    -- internal ones in there.
    valid_control cc = cc>0 && (cc<11 || cc>15) && cc<120
    -- TODO 120 is aftertouch, which I could support if ControlMap did

    process_controls :: [(String, Midi.Control, [Word8])]
        -> [(Midi.Control, [String])]
    process_controls controls =
        [(cc, map snd grp) | (cc, grp) <- Seq.keyed_group_on fst by_cc]
        where
        by_cc = [(cc, name) | (name, cc, depths) <- controls, valid_control cc,
            maximum (map abs depths) >= 32]

-- | Vaguely \"more audible\" controls come first.  Having more than one seq
-- control affecting the same vl1 control is confusing, so when a control is
-- assigned to more than one control, the one first in this list will get
-- the control.  That way, if contoller 2 is assigned to both pressure and
-- amplitude, the control will be called @pressure@.
--
-- Of course prominence is also highly dependent on depth, but this is simpler.
-- I ignore controls below a certain depth anyway.
--
-- Paired with the byte offset in the @element parameters@ sysex section.
vl1_control_map :: [(String, Bool)]
vl1_control_map =
    [ ("embouchure", True)
    , ("pressure", False)
    , ("amplitude", False)
    , ("scream", False)
    , ("growl", False)
    , ("vibrato", False)
    , ("dynamic-filter", False)
    , ("throat-formant", False)
    , ("breath-noise", False)
    , ("harmonic-enhancer", False)
    , ("tonguing", False)
    , ("damping", False)
    , ("absorption", False)
    ]


-- * config

-- | From the Vl1m MIDI spec:
--
-- > 0-127 => 00 - 7f
-- > 0-127, 128-16383      => 0000 - 007f, 0100 - 7f7f
-- > -64 - -1, 0, 1 - 63   => 40 - 7f, 00, 01 - 3f (2s complement)
-- > -128 - -1, 0, 1 - 127 => 0100 - 017f, 0000, 0001 - 007f
config :: Sysex.Config
config = Sysex.Config decode_num encode_num range_bytes

decode_num :: Sysex.NumRange -> ByteString -> Int
decode_num (low, high) bytes
    | low >= 0 = val
    | high <= 0x3f = Sysex.to_signed 7 val
    | otherwise = val .&. 0x7f - val .&. 0x80
    where
    val = B.foldl' (\val b -> Bits.shiftL val 7 .|. fromIntegral b) 0 bytes

encode_num :: Sysex.NumRange -> Int -> ByteString
encode_num (low, high) num
    | low >= 0 = B.reverse $ B.unfoldr unfold (num, range_bytes (low, high))
    | range_bytes (low, high) == 1 = B.singleton $ Sysex.from_signed 7 num
    | otherwise =
        let b = Sysex.from_signed 8 num
        in B.pack [Bits.shiftR b 7, b .&. 0x7f]
    where
    unfold (num, left)
        | left > 0 =
            Just (fromIntegral (num .&. 0x7f), (Bits.shiftR num 7, left-1))
        | otherwise = Nothing

range_bytes :: Sysex.NumRange -> Int
range_bytes (low, high) = ceiling $ logBase 2 (fromIntegral range + 1) / 7
    where range = if low < 0 then high - low else high

decode :: Specs -> ByteString -> Either String (Sysex.RMap, ByteString)
decode = Sysex.decode config

encode :: Specs -> Sysex.RMap -> Either String ByteString
encode = Sysex.encode config

-- * specs

patch_spec :: Specs
patch_spec = Sysex.assert_valid "patch_spec"
    [ ("header", Constant (vl1_header 3084))
    , ("memory type", unsigned 127) -- 0 = memory, 7f = buffer
    , ("memory number", unsigned 127)
    , ("", Unparsed 14)
    ] ++ common_spec

common_spec :: Specs
common_spec =
    [ ("name", Str 10)
    , ("", Unparsed 6)
    , ("key mode", enum ["mono", "unison", "poly", "part"])
    , ("voice mode", enum ["single", "dual"])
    , ("split mode", unsigned 2)
    , ("split point", unsigned 127)
    , ("split interval", unsigned 24)
    , ("elem1 midi rch", unsigned 15)
    , ("elem2 midi rch", unsigned 15)
    , ("poly expand mode", unsigned 31)
    , ("poly expand no", unsigned 31)
    , ("pb, at & mod mode", unsigned 2)
    , ("polyphony control", unsigned 119)
    , ("sustain", bool)
    , ("pitch bend mode", unsigned 2)
    , ("assign mode", unsigned 2)
    , ("breath attack time", unsigned 127)
    , ("breath attack gain", unsigned 127)
    , ("touch eg time", unsigned 127)
    , ("touch eg gain", unsigned 127)
    , ("portamento time midi control", bool)
    , ("portamento mode", unsigned 1)
    , ("portamento time", unsigned 127)
    , ("elem1 portamento", bool)
    , ("elem2 portamento", bool)
    , ("elem1 detune", signed 7)
    , ("elem2 detune", signed 7)
    , ("elem1 note shift", ranged (-64) 63)
    , ("elem2 note shift", ranged (-64) 63)
    , ("elem1 rand pitch", unsigned 7)
    , ("elem2 rand pitch", unsigned 7)
    , ("elem1 microtuning", unsigned 86)
    , ("elem2 microtuning", unsigned 86)
    , ("elem1 level", unsigned 127)
    , ("elem2 level", unsigned 127)
    , ("elem1 pan l", ranged (-64) 63)
    , ("elem1 pan r", ranged (-64) 63)
    , ("elem2 pan l", ranged (-64) 63)
    , ("elem2 pan r", ranged (-64) 63)
    , ("cs1 class", unsigned 4)
    , ("cs1 assign", unsigned 150)
    , ("cs2 class", unsigned 4)
    , ("cs2 assign", unsigned 150)
    , ("destination effect", unsigned 3)
    , ("destination controller", unsigned 122)
    , ("effect type", enum
        [ "flanger", "pitch change", "distortion", "chorus", "phaser"
        , "symphonic", "celeste", "distortion+flanger", "distortion+wah"
        ])
    , ("elem1 on", enum ["on", "off"]) -- naturally 0 is on, 1 is off
    , ("elem2 on", enum ["on", "off"])
    , ("effect", Unparsed 10)
    , ("feedback/reverb mode", bool)
    -- TODO just guessing about off
    , ("feedback type", enum ["off", "mono", "l/r", "l/c/r"])
    , ("feedback return", unsigned 100)
    , ("feedback data", Unparsed 18)
    , ("reverb type", unsigned 8) -- presumably an enum
    , ("reverb data", Unparsed 10)
    , ("", Unparsed 2)
    , ("element", List 2 element_spec)
    ]

element_spec :: Specs
element_spec = controls_spec ++
    [ ("unparsed1", Unparsed (231 - Sysex.spec_bytes config controls_spec))
    , ("name", Str 10)
    , ("unparsed2", Unparsed (total - 240)) -- or 242?
    ]
    where total = 1479

controls_spec :: Specs
controls_spec =
    [ ("pressure", c_simple)
    , ("embouchure", SubSpec
        [c_control, c_mode, ("upper depth", depth), ("lower depth", depth)])
    , ("pitch", SubSpec
        [ c_control, c_mode
        , ("upper depth", signed 12), ("lower depth", signed 12)
        ])
    , ("vibrato", SubSpec [c_control, ("", Unparsed 1), c_depth])
    , ("tonguing", c_simple)
    , ("amplitude", c_simple)
    , ("scream", c_complete)
    , ("breath-noise", c_complete)
    , ("growl", c_complete)
    , ("throat-formant", c_complete)
    , ("dynamic-filter", c_simple)
    , ("harmonic-enhancer", c_simple)
    , ("damping", c_simple)
    , ("absorption", c_simple)
    ]
    where
    c_simple = SubSpec [c_control, c_curve, c_depth]
    c_complete = SubSpec [c_control, c_value, c_curve, c_depth]

    -- I think if the control is 127, then this control is disabled and
    -- the rest can be ignored.
    -- c_control = ("control", unsigned 127) -- 124)
    c_control = ("control", unsigned 124)
    c_value = ("value", unsigned 127)
    c_mode = ("mode", enum ["mode1", "mode2"]) -- TODO what are these really?
    c_curve = ("curve", signed 16)
    c_depth = ("depth", depth)
    depth = signed 127


-- * modulation effects

wet_dry :: (String, Sysex.Spec)
wet_dry = ("wet/dry balance", unsigned 100)

flanger_specs :: Specs
flanger_specs =
    [ ("wave", unsigned 2)
    , ("freq", unsigned 127)
    , ("depth", unsigned 100)
    , ("delay", unsigned 126)
    , ("phase", signed 8)
    , ("fb gain", signed 100)
    , ("high", unsigned 9)
    , ("analog feel", unsigned 10)
    , wet_dry
    ]

pitch_change_specs :: Specs
pitch_change_specs =
    [ ("mode", unsigned 1) -- enum
    , ("pitch 1", signed 12)
    , ("fine 1", signed 100)
    , ("out 1", unsigned 100)
    , ("pitch 2", signed 12)
    , ("fine 2", signed 100)
    , ("out 2", unsigned 100)
    , wet_dry
    ]

distortion_specs :: Specs
distortion_specs =
    [ ("overdrive", unsigned 100)
    , ("", Unparsed 2)
    , ("device", unsigned 4)
    , ("speaker", unsigned 5)
    , ("presence", signed 10)
    , ("output level", unsigned 100)
    ]

chorus_specs :: Specs
chorus_specs =
    [ ("mode", unsigned 1) -- enum
    , ("freq", unsigned 127)
    , ("depth", unsigned 100)
    , ("delay", unsigned 126)
    , ("fb gain", signed 100)
    , ("high", unsigned 9)
    , wet_dry
    ]

phaser_specs :: Specs
phaser_specs =
    [ ("mode", unsigned 1) -- enum
    , ("stage", unsigned 3) -- enum
    , ("freq", unsigned 127)
    , ("depth", unsigned 100)
    , ("offset", unsigned 100)
    , ("phase", signed 8) -- diffusion 0--1 when mode=0
    , ("fb gain", signed 100)
    , wet_dry
    ]

symphonic_specs :: Specs
symphonic_specs =
    [ ("mode", unsigned 1) -- enum
    , ("freq", unsigned 127)
    , ("depth", unsigned 100)
    , ("diffusion", unsigned 10)
    , ("lo-fi", unsigned 12)
    , wet_dry
    ]

celeste_specs :: Specs
celeste_specs =
    [ ("mode", unsigned 1)
    , ("freq", unsigned 127)
    , ("depth", unsigned 100)
    , ("delay", unsigned 126)
    , ("fb gain", signed 100)
    , ("lo-fi", unsigned 12)
    , wet_dry
    ]

distortion_flanger_specs :: Specs
distortion_flanger_specs =
    [ ("overdrive", unsigned 100)
    , ("speaker", unsigned 5)
    , ("output level", unsigned 100)
    , ("freq", unsigned 127)
    , ("depth", unsigned 100)
    , ("delay", unsigned 126)
    , ("phase", signed 8)
    , ("fb gain", unsigned 100)
    , ("high", unsigned 9)
    , ("flanger balance", unsigned 100)
    ]

distortion_wah_specs :: Specs
distortion_wah_specs =
    [ ("overdrive", unsigned 100)
    , ("speaker", unsigned 5)
    , ("output level", unsigned 100)
    , ("mode", unsigned 3)
    , ("wah pre/post", unsigned 1)
    , ("cutoff freq", unsigned 127)
    , ("resonance", unsigned 127)
    , ("sensitivity", unsigned 100)
    ]
