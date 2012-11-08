{-# LANGUAGE OverloadedStrings #-}
-- | Yamaha VL1 synthesizer.
module Local.Instrument.Vl1m where
import qualified Data.Bits as Bits
import Data.Bits ((.|.), (.&.))
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Word (Word8)

import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified Text.Parsec as Parsec

import Util.Control
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Parse as Parse
import qualified Instrument.Sysex as Sysex
import Instrument.Sysex
       (Specs, Spec(..), unsigned, bool, enum, ranged)

import qualified App.MidiInst as MidiInst


name :: String
name = "vl1"

load :: FilePath -> IO [MidiInst.SynthDesc]
load = MidiInst.load_db (const MidiInst.empty_code) name

-- | Read the patch file, scan the sysex dir, and save the results in a cache.
make_db :: FilePath -> IO ()
make_db dir = do
    vc_syxs <- parse_dir (dir </> name </> "vc")
    syxs <- parse_dir (dir </> name </> "sysex")
    patches <- Parse.patch_file (dir </> name </> "patches")
    MidiInst.save_patches synth (vc_syxs ++ syxs ++ patches) name dir

synth :: Instrument.Synth
synth = Instrument.synth name []

-- * spec

test = do
    syxs <- file_to_syx "patchman1.syx"
    return $ parse_sysex (head syxs)

parse_sysex :: ByteString -> Either String Sysex.Record
parse_sysex bytes = do
    Sysex.expect_bytes bytes $ B.pack [0xf0, Midi.yamaha_code, 0]
    fst <$> decode patch_spec bytes

file_to_syx :: FilePath -> IO [ByteString]
file_to_syx fn = case FilePath.takeExtension fn of
        ".all" -> split_all <$> B.readFile fn
        ".1vc" -> split_1vc <$> B.readFile fn
        ".1bk" -> split_1bk <$> B.readFile fn
        ".syx" -> split_syx <$> B.readFile fn
        ".txt" -> return []
        _ -> return [] -- TODO log warn
    where
    -- | Convert .1vc format to .syx format.  Derived by looking at vlone70
    -- conversions with od.
    split_1vc bytes = [bytes_to_syx (B.drop 0xc00 bytes)]
    split_1bk = map bytes_to_syx . split
        where
        split bytes = takeWhile (not . B.all (==0) . B.take 20) $
            map (flip B.drop bytes) offsets
        offsets = [0xc00, 0x1800..]
    split_all = split_1bk -- turns out they're the same
    split_syx = B.split Midi.eox_byte

-- | Wrap sysex codes around the raw bytes.
bytes_to_syx :: ByteString -> ByteString
bytes_to_syx bytes = vl1_header
    <> B.pack [0x7f, 0] -- memory type, memory number
    <> B.replicate 14 0 -- padding
    <> B.take size bytes
    -- 0x42 is the checksum, but vl1 doesn't seem to care if it's right or
    -- not.
    <> B.pack [0x42, Midi.eox_byte]
    where size = 0xc1c - 0x20


vl1_header :: ByteString
vl1_header = B.pack
    [ 0xf0, Midi.yamaha_code, 0, 0x7a
    , 0x18, 0x16 -- byte count
    ] <> "LM 20117VC"


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

decode :: Specs -> ByteString -> Either String (Sysex.Record, ByteString)
decode = Sysex.decode config

encode :: Specs -> Sysex.Record -> Either String ByteString
encode = Sysex.encode config

-- * specs

patch_spec :: Specs
patch_spec = Sysex.assert_valid "patch_spec"
    [ ("header", Unparsed 6)
    , ("sysex type", Str 10)
    , ("memory type", unsigned 127)
    , ("memory number", unsigned 127)
    , ("", Unparsed 14)
    ] ++ common_spec

common_spec :: Specs
common_spec =
    [ ("name", Str 10)
    , ("", Unparsed 6)
    , ("key mode", unsigned 3)
    , ("voice mode", unsigned 1)
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
    , ("elem1 detune", ranged (-7) 7)
    , ("elem2 detune", ranged (-7) 7)
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
    , ("elem1 on", bool)
    , ("elem2 on", bool)
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
element_spec =
    [ ("unparsed", Unparsed 1479)
    ]


-- * old

parse_dir :: FilePath -> IO [Instrument.Patch]
parse_dir dir = do
    fns <- File.recursive_list_dir (const True) dir
    parses <- fmap concat $ mapM parse_file fns
    Parse.warn_parses parses

parse_file :: FilePath -> IO [Either Parsec.ParseError Instrument.Patch]
parse_file fn = do
    syxs <- case FilePath.takeExtension fn of
        -- The patchman .all dumps have 128 patches, but only the first 64
        -- have valid instruments.
        ".all" | "patchman" `List.isInfixOf` fn ->
            take 64 . syx_all <$> File.read_binary fn
        ".1vc" -> syx_1vc <$> File.read_binary fn
        ".1bk" -> syx_1bk <$> File.read_binary fn
        ".syx" -> syx_split <$> File.read_binary fn
        ".txt" -> return []
        _ -> Log.warn ("skipping " ++ show fn) >> return []
    txt <- fromMaybe "" <$>
        File.ignore_enoent (readFile (FilePath.replaceExtension fn ".txt"))
    return $ map (parse fn txt) syxs

parse :: FilePath -> String -> [Word8]
    -> Either Parsec.ParseError Instrument.Patch
parse fn txt syx = combine fn txt syx <$> Parse.parse_sysex vl1_sysex fn syx

combine :: FilePath -> String -> [Word8] -> Instrument.Patch
    -> Instrument.Patch
combine fn txt syx patch = Sysex.add_file fn $ patch
    { Instrument.patch_text = Seq.strip txt
    , Instrument.patch_initialize = Parse.make_sysex_init syx
    }

-- | Convert .1vc format to .syx format.  Derived by looking at vlone70
-- conversions with od.
syx_1vc :: [Word8] -> [[Word8]]
syx_1vc bytes = [words_to_syx (drop 0xc00 bytes)]

syx_1bk :: [Word8] -> [[Word8]]
syx_1bk = map words_to_syx . split_1bk
    where
    split_1bk bytes =
        takeWhile (not . all (==0) . take 20) $ map (flip drop bytes) offsets
    offsets = [0xc00, 0x1800..]

syx_all :: [Word8] -> [[Word8]]
syx_all = syx_1bk -- turns out they're the same

syx_split :: [Word8] -> [[Word8]]
    -- The first elt is stuff before the SOX so it's probably null.
syx_split = drop 1 . Seq.split_with (==Midi.sox_byte)

-- | Wrap sysex codes around the raw bytes.
words_to_syx :: [Word8] -> [Word8]
words_to_syx bytes = syx_bytes ++ [checksum syx_bytes, 0xf7]
    where
    size = 0xc1c - 0x20
    syx_bytes =
        [ 0xf0, Midi.yamaha_code, 0, 0x7a
        , 0x18, 0x16 -- byte count
        ] ++ map (fromIntegral . fromEnum) ("LM 20117VC" :: String) ++
        [ 0x7f, 0 ] -- memory type, memory number
        ++ replicate 14 0 -- padding
        ++ take size bytes
    checksum _ = 0x42 -- vl1 doesn't seem to care if this is right or not

vl1_sysex :: Parse.ByteParser Instrument.Patch
vl1_sysex = do
    Parse.start_sysex Midi.yamaha_code
    Parse.one_byte -- device num
    Parse.match_bytes [0x7a]
    Parse.n_bytes 2 -- byte count
    Parse.match_bytes (map (fromIntegral . fromEnum) ("LM 20117VC" :: String))
    Parse.one_byte -- memory type
    Parse.one_byte -- memory number
    Parse.n_bytes 14 -- nulls
    common <- fmap common_data (Parse.n_bytes 108) -- 32~139
    elt1 <- fmap element (Parse.n_bytes 1480) -- 140~1619
    elt2 <- fmap element (Parse.n_bytes 1480) -- 1620~3099
    return $ vl1_patch common elt1 elt2
    -- Parse.end_sysex
    where
    common_data :: [Word8] -> String
    common_data = Seq.strip . Parse.to_string . take 10

vl1_patch :: Instrument.InstrumentName -> ElementInfo -> ElementInfo
    -> Instrument.Patch
vl1_patch name (pb_range1, name1, cc_groups1) (pb_range2, name2, cc_groups2) =
    -- Initialization and text will be filled in later.
    (Instrument.patch inst) { Instrument.patch_tags = tags }
    where
    -- Optimistically take the widest range.
    pb_range = if range pb_range1 > range pb_range2
        then pb_range1 else pb_range2
    range (low, high) = max (abs low) (abs high)
    inst = Instrument.instrument name cmap pb_range
    tags = maybe_tags [("vl1_elt1", name1), ("vl1_elt2", name2)]
    cmap = Map.assocs $ Map.mapMaybe highest_prio $
        Map.unionWith (++) (Map.fromList cc_groups1) (Map.fromList cc_groups2)
    highest_prio cs = List.find (`elem` cs) control_prios

maybe_tags :: [(String, String)] -> [Instrument.Tag]
maybe_tags = filter (not . null . snd)

-- | Each voice has two elements, each with their own PbRange, name, and
-- controls.
type ElementInfo = (Control.PbRange, String, [(Midi.Control, [String])])

element :: [Word8] -> ElementInfo
element bytes = ((pb_up, pb_down), name, c_groups)
    where
    (pb_up, pb_down) =
        (Parse.from_signed_7bit (bytes!!12),
            Parse.from_signed_7bit (bytes!!13))
    -- doc says 231~240
    name = Seq.strip $ Parse.to_string $ take 10 $ drop 231 bytes
    controls = mapMaybe (get_control bytes) vl1_control_map
    c_groups = [(cc, map fst grp)
        | (cc, grp) <- Seq.keyed_group_on snd controls]

get_control :: [Word8] -> Vl1Control -> Maybe (String, Midi.Control)
get_control bytes (name, offset, depth, upper_lower) = do
    midi_control <- require valid_control control
    require (>=32) $ maximum $ map abs depth_bytes
    return (name, midi_control)
    where
    control = fromIntegral (bytes !! offset)
    depth_bytes = [get_7bit bytes (offset+depth)]
        ++ if upper_lower then [get_7bit bytes (offset+depth+2)] else []
    require f v = if f v then Just v else Nothing
        -- The vl1 mostly uses the midi control list, except sticks some
        -- internal ones in there.
    valid_control c = c>0 && (c<11 || c>15) && c<120
    -- TODO 120 is aftertouch, which I could support if ControlMap did

get_7bit :: [Word8] -> Int -> Int
get_7bit bytes offset = Parse.from_signed_8bit (Midi.join14 msb lsb)
    where [msb, lsb] = take 2 (drop offset bytes)

-- | (name, sysex_offset, depth_offset, has_upper_lower)
type Vl1Control = (String, Int, Int, Bool)

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
vl1_control_map :: [Vl1Control]
vl1_control_map =
    [ ("embouchure", 4, 2, True)
    , ("pressure", 0, 2, False)
    , ("amplitude", 22, 2, False)

    , ("scream", 26, 3, False)
    , ("growl", 36, 3, False)
    , ("vibrato", 14, 2, False)

    , ("dynamic-filter", 46, 2, False)
    , ("throat-formant", 41, 3, False)

    , ("breath-noise", 31, 3, False)
    , ("harmonic-enhancer", 50, 2, False)

    , ("tonguing", 18, 2, False)
    , ("damping", 54, 2, False)
    , ("absorption", 58, 2, False)
    ]

control_prios :: [String]
control_prios = [c | (c, _, _, _) <- vl1_control_map]
