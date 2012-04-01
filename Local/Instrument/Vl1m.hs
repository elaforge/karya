-- | Yamaha VL1 synthesizer.
module Local.Instrument.Vl1m where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
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
import qualified App.MidiInst as MidiInst


db_name :: String
db_name = "vl1"

load :: FilePath -> IO [MidiInst.SynthDesc]
load = MidiInst.load_db (const MidiInst.empty_code) db_name

-- | Read the patch file, scan the sysex dir, and save the results in a cache.
make_db :: FilePath -> IO ()
make_db dir = do
    vc_syxs <- parse_dir (dir </> "vl1_vc")
    syxs <- parse_dir (dir </> "vl1_syx")
    patches <- Parse.patch_file (dir </> "vl1")
    MidiInst.save_patches synth (vc_syxs ++ syxs ++ patches) db_name dir

synth :: Instrument.Synth
synth = Instrument.synth "vl1" []

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
    txt <- fmap (maybe "" id) $
        File.ignore_enoent (readFile (FilePath.replaceExtension fn ".txt"))
    return $ map (parse fn txt) syxs

parse :: FilePath -> String -> [Word8]
    -> Either Parsec.ParseError Instrument.Patch
parse fn txt syx = combine fn txt syx <$> Parse.parse_sysex vl1_sysex fn syx

combine :: FilePath -> String -> [Word8] -> Instrument.Patch
    -> Instrument.Patch
combine fn txt syx patch = Parse.add_file fn $ patch
    { Instrument.patch_text = Seq.strip txt
    , Instrument.patch_initialize = Parse.make_sysex_init syx
    }

-- | Convert .1vc format to .syx format.  Derived by looking at vlone70
-- conversions with od.
syx_1vc :: [Word8] -> [[Word8]]
syx_1vc bytes = [bytes_to_syx (drop 0xc00 bytes)]

syx_1bk :: [Word8] -> [[Word8]]
syx_1bk = map bytes_to_syx . split_1bk
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
bytes_to_syx :: [Word8] -> [Word8]
bytes_to_syx bytes = syx_bytes ++ [checksum syx_bytes, 0xf7]
    where
    size = 0xc1c - 0x20
    syx_bytes =
        [ 0xf0, Parse.yamaha_code, 0, 0x7a
        , 0x18, 0x16 -- byte count
        ] ++ map (fromIntegral . fromEnum) "LM 20117VC" ++
        [ 0x7f, 0 ] -- memory type, memory number
        ++ replicate 14 0 -- padding
        ++ take size bytes
    checksum _ = 0x42 -- vl1 doesn't seem to care if this is right or not

vl1_sysex :: Parse.ByteParser Instrument.Patch
vl1_sysex = do
    Parse.start_sysex Parse.yamaha_code
    Parse.one_byte -- device num
    Parse.match_bytes [0x7a]
    Parse.n_bytes 2 -- byte count
    Parse.match_bytes (map (fromIntegral . fromEnum) "LM 20117VC")
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
maybe_tags tags = [Instrument.tag k v | (k, v) <- tags, not (null v)]

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
    controls = Maybe.mapMaybe (get_control bytes) vl1_control_map
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

get_7bit :: [Word8] -> Int -> Integer
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
