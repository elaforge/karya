{- | Instrument db for the Yamaha VL1-m.
-}
module Local.Instrument.Vl1m where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word
import System.FilePath ((</>))
import qualified System.FilePath as FilePath

import qualified Util.File as File
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Controller as Controller

import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Parse as Parse


load dir = Parse.patch_file (dir </> "vl1") >>= MidiDb.load_synth_desc vl1
load_slow dir = parse_dir (dir </> "vl1_vc") >>= MidiDb.load_synth_desc vl1

vl1 = Instrument.synth "vl1" "vl1" vl1_controllers
vl1_controllers = []

parse_dir :: FilePath -> IO [Instrument.Patch]
parse_dir dir = do
    fns <- File.recursive_list_dir (const True) dir
    parses <- fmap concat $ mapM parse_file fns
    Parse.warn_parses parses

parse_file fn = do
    syxs <- case FilePath.takeExtension fn of
        ".1vc" -> fmap ((:[]) . _1vc_to_syx) $ File.read_binary fn
        ".1bk" -> fmap _1bk_to_syxs $ File.read_binary fn
        ".txt" -> return []
        _ -> putStrLn ("Vl1m: skipping " ++ show fn) >> return []
    txt <- fmap (maybe "" id) $
        File.catch_enoent (readFile (FilePath.replaceExtension fn ".txt"))
    return $ map (parse fn txt) syxs

parse fn txt syx = case Parse.parse_sysex vl1_sysex fn syx of
    Left err -> Left err
    Right patch -> Right (combine fn txt syx patch)

combine :: FilePath -> String -> [Word.Word8] -> Instrument.Patch
    -> Instrument.Patch
combine fn txt syx patch = patch
    { Instrument.patch_text = Seq.strip txt ++ "\n\nFile: " ++ fn
    , Instrument.patch_initialize = Parse.make_sysex_init syx
    }

-- | Convert .1vc format to .syx format.  Derived by looking at vlone70
-- conversions with od.
_1vc_to_syx :: [Word.Word8] -> [Word.Word8]
_1vc_to_syx bytes = convert_bytes (drop 0xc00 bytes)

_1bk_to_syxs :: [Word.Word8] -> [[Word.Word8]]
_1bk_to_syxs = map convert_bytes . split_1bk

split_1bk bytes = takeWhile (not . all (==0) . take 20) $
        map (flip drop bytes) offsets
    where offsets = [0xc00, 0x1800..]

-- | Wrap sysex codes around the raw bytes.
convert_bytes bytes = syx_bytes ++ [checksum syx_bytes, 0xf7]
    where
    size = 0xc1c - 0x20
    syx_bytes =
        [ 0xf0, Parse.yamaha_code, 0, 0x7a
        , 0x18, 0x16 -- byte count
        ] ++ map (fromIntegral . fromEnum) "LM 20117VC" ++
        [ 0x7f, 0 ] -- memory type, memory number
        ++ replicate 14 0 -- padding
        ++ take size bytes
    checksum _ = 0x42 -- TODO

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

vl1_patch name (pb_range1, name1, cc_groups1) (pb_range2, name2, cc_groups2) =
    -- Initialization and text will be filled in later.
    Instrument.patch inst Instrument.NoInitialization tags ""
    where
    pb_range = if range pb_range1 > range pb_range2
        then pb_range1 else pb_range2
    range (low, high) = max (abs low) (abs high)
    inst = Instrument.instrument name cmap pb_range Nothing
    tags = maybe_tags [("vl1_elt1", name1), ("vl1_elt2", name2)]
    cmap = Controller.controller_map $ Map.assocs $ Map.mapMaybe highest_prio $
        Map.unionWith (++) (Map.fromList cc_groups1) (Map.fromList cc_groups2)
    highest_prio cs = List.find (`elem` cs) controller_prios

maybe_tags tags = [Instrument.tag k v | (k, v) <- tags, not (null v)]

common_data bytes = name
    where
    name = Seq.strip $ Parse.to_string (take 10 bytes)

element :: [Word.Word8] -> (Controller.PbRange, String, [(Integer, [String])])
element bytes = ((pb_up, pb_down), name, c_groups)
    where
    (pb_up, pb_down) =
        (Parse.from_signed_7bit (bytes!!12), Parse.from_signed_7bit (bytes!!13))
    -- doc says 231~240
    name = Seq.strip $ Parse.to_string $ take 10 $ drop 231 bytes
    controls = Maybe.catMaybes $ map (get_controller bytes) controllers
    c_groups = [(cc, map fst grp)
        | (cc, grp) <- Seq.keyed_group_with snd controls]

get_controller :: [Word.Word8] -> Vl1Control -> Maybe (String, Integer)
get_controller bytes (name, offset, depth, upper_lower) = do
    midi_control <- require valid_control control
    require (>=32) $ maximum $ map abs depth_bytes
    return (name, midi_control)
    where
    control = fromIntegral (bytes !! offset)
    depth_bytes = [get_7bit bytes (offset+depth)]
        ++ if upper_lower then [get_7bit bytes (offset+depth+2)] else []
    require f v = if f v then Just v else Nothing
        -- The vl1 mostly uses the midi controller list, except sticks some
        -- internal ones in there.
    valid_control c = c>0 && (c<11 || c>15) && c<120
    -- TODO 120 is aftertouch, which I could support if ControllerMap did

get_7bit bytes offset = Parse.from_signed_8bit (Midi.join14 msb lsb)
    where [msb, lsb] = take 2 (drop offset bytes)

-- | (name, sysex_offset, depth_offset, has_upper_lower)
type Vl1Control = (String, Int, Int, Bool)

-- | Vaguely "more audible" controls come first.  Having more than one seq
-- control affecting the same vl1 control is confusing, so when a control is
-- assigned to more than one controller, the one first in this list will get
-- the controller.  That way, if contoller 2 is assigned to both pressure and
-- amplitude, the controller will be called "pressure".
--
-- Of course prominence is also highly dependent on depth, but this is simpler.
-- I ignore controls below a certain depth anyway.
--
-- Paired with the byte offset in the "element parameters" sysex section.
controllers :: [Vl1Control]
controllers =
    [ ("embouchure", 4, 2, True)
    , ("pressure", 0, 2, False)
    , ("amplitude", 22, 2, False)

    , ("scream", 26, 3, False)
    , ("growl", 36, 3, False)
    , ("vibrato", 14, 2, False)

    , ("dynamic filter", 46, 2, False)
    , ("throat formant", 41, 3, False)

    , ("breath noise", 31, 3, False)
    , ("harmonic enhancer", 50, 2, False)

    , ("tonguing", 18, 2, False)
    , ("damping", 54, 2, False)
    , ("absorption", 58, 2, False)
    ]
controller_prios = [c | (c, _, _, _) <- controllers]
