-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Yamaha VL1 synthesizer.
module User.Elaforge.Instrument.Vl1 where
import           Data.Bits ((.&.))
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import           Data.Word (Word8)

import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified Text.Printf as Printf

import qualified Util.Doc as Doc
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import qualified Instrument.Sysex as Sysex

import qualified Midi.CC as CC
import qualified Midi.Encode
import qualified Midi.Midi as Midi

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import qualified User.Elaforge.Instrument.Vl1Spec as Vl1Spec

import           Global


synth_name :: InstTypes.SynthName
synth_name = "vl1"

load :: Path.AppDir -> IO (Maybe MidiInst.Synth)
load = MidiInst.load_synth (const mempty) synth_name "Yamaha Vl1"

-- | Read the patch file, scan the sysex dir, and save the results in a cache.
make_db :: Path.AppDir -> IO ()
make_db app_dir = do
    let dir = Path.to_absolute app_dir Config.instrument_dir
            </> untxt synth_name
    let dirs = map (dir</>) ["vc", "sysex", "patchman1", "patchman2"]
    patches <- concatMapM parse_dir dirs
    builtins <- parse_builtins (dir </> builtin)
    MidiInst.save_synth app_dir synth_name (builtins ++ patches)

builtin :: FilePath
builtin = "vl1v2-factory/vl1_ver2.all"


-- * parse

-- | Write .syx and .rec files for the contents of the given file.
extract_syxs :: FilePath -> FilePath -> IO ()
extract_syxs dir fn = do
    syxs <- file_to_syx fn
    forM_ (zip [0..] syxs) $ \(n, syx) -> do
        let Right rec = decode_sysex syx
            Right name = Sysex.get_rmap "name" rec
            fn = dir </> syx_fname n name
        B.writeFile (fn ++ ".syx") syx
        writeFile (fn ++ ".rec") (unlines (Sysex.show_flat rec))

syx_fname :: Int -> Text -> FilePath
syx_fname num name =
    Printf.printf "%03d.%s" num (untxt $ MidiInst.clean_name name)

send_to_buffer = modify
    [ put_int "memory type" 0x7f
    , put_int "memory number" 0
    ]

send_to_patch num = modify
    [ put_int "memory type" 0
    , put_int "memory number" num
    ]

set_pitch_bend = modify
    [ put_int "element.0.control.pitch.lower depth" (-12)
    , put_int "element.0.control.pitch.upper depth" 12
    , put_int "element.1.control.pitch.lower depth" (-12)
    , put_int "element.1.control.pitch.upper depth" 12
    ]

modify :: Monad m => [a -> m a] -> a -> m a
modify = foldr (<=<) return

put_int :: String -> Int -> Sysex.RMap -> Either String Sysex.RMap
put_int path int = Sysex.put_rmap path int

parse_builtins :: FilePath -> IO [MidiInst.Patch]
parse_builtins fn = do
    (warns, patches) <- Either.partitionEithers <$> parse_file fn
    mapM_ (Log.warn . txt) warns
    return $ zipWith initialize [0..] patches
    where
    initialize n = MidiInst.patch#Patch.initialize
        #= Patch.initialize_midi (Midi.program_change 0 n)

parse_dir :: FilePath -> IO [MidiInst.Patch]
parse_dir dir = do
    fns <- File.listRecursive (const True) dir
    (warns, patches) <- Either.partitionEithers . concat <$> mapM parse_file fns
    mapM_ (Log.warn . txt) warns
    return patches

parse_file :: FilePath -> IO [Either String MidiInst.Patch]
parse_file fn = do
    syxs <- file_to_syx fn
    doc <- fromMaybe "" <$> File.ignoreEnoent
        (Text.IO.readFile (FilePath.replaceExtension fn ".txt"))
    let results = map (record_to_patch <=< decode_sysex) syxs
    return
        [ bimap (failed i) (combine fn doc syx) result
        | (i, syx, result) <- zip3 [1..] syxs results
        ]
    where
    failed i msg = "parsing " ++ show fn ++ "/" ++ show i ++ ": " ++ msg

combine :: FilePath -> Text -> ByteString -> MidiInst.Patch -> MidiInst.Patch
combine fn doc syx =
    (MidiInst.common %= Sysex.add_file fn)
    . (MidiInst.doc #= Doc.Doc (Text.strip doc))
    . (MidiInst.patch#Patch.initialize #=
        Patch.InitializeMidi [Midi.Encode.decode syx])

decode_sysex :: ByteString -> Either String Sysex.RMap
decode_sysex bytes = fst <$> Vl1Spec.decode Vl1Spec.patch_spec bytes

encode_sysex :: Sysex.RMap -> Either String ByteString
encode_sysex = fmap append_suffix . Vl1Spec.encode Vl1Spec.patch_spec

file_to_syx :: FilePath -> IO [ByteString]
file_to_syx fn = map add_extra_zero <$> case FilePath.takeExtension fn of
    ".all" -> split_1bk Nothing <$> B.readFile fn
    ".1vc" -> split_1vc <$> B.readFile fn
    ".1bk" -> split_1bk Nothing <$> B.readFile fn
    ".syx" -> split_syx <$> B.readFile fn
    ".txt" -> return []
    ".rec" -> return []
    _ -> Log.warn ("skipping " <> showt fn) >> return []
    where
    -- | Convert .1vc format to .syx format.  Derived by looking at vlone70
    -- conversions with od.
    split_1vc bytes = [bytes_to_syx Nothing (B.drop 0xc00 bytes)]
    split_syx = map (<> B.singleton Midi.Encode.eox_byte)
        . filter (not . B.null) . B.split Midi.Encode.eox_byte

split_1bk :: Maybe Word8 -> ByteString -> [ByteString]
split_1bk memory = zipWith (\n -> bytes_to_syx ((+n) <$> memory)) [0..] . split
    where
    split bytes = takeWhile (not . B.all (==0) . B.take 20) $
        map (flip B.drop bytes) offsets
    offsets = [0xc00, 0x1800..]

-- | For some reason, some sysexes come out with a 0 for device numbers, and
-- some omit it entirely.
add_extra_zero :: ByteString -> ByteString
add_extra_zero bytes
    | B.isPrefixOf short bytes = long <> B.drop (B.length short) bytes
    | otherwise = bytes
    where
    long = B.pack [0xf0, Midi.yamaha_code, 0, 0x7a]
    short = B.pack [0xf0, Midi.yamaha_code, 0x7a]

-- | Wrap sysex codes around the raw bytes.
bytes_to_syx :: Maybe Word8 -> ByteString -> ByteString
bytes_to_syx memory bytes = append_suffix $
    Vl1Spec.vl1_header (2 + 14 + size)
        -- memory type, memory number
        <> B.pack (maybe [0x7f, 0] (\n -> [0, n]) memory)
        <> B.replicate 14 0 -- padding
        <> B.take size bytes
    where size = 0xc1c - 0x20

append_suffix :: ByteString -> ByteString
append_suffix bytes = bytes <> B.pack [checksum, Midi.Encode.eox_byte]
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
type ElementInfo = (Control.PbRange, Text, [(Midi.Control, [ScoreT.Control])])

record_to_patch :: Sysex.RMap -> Either String MidiInst.Patch
record_to_patch rmap = do
    name <- get "name"
    elt1 <- extract_element 0 rmap
    maybe_elt2 <- ifM ((== ("dual" :: Text)) <$> get "voice mode")
        (Just <$> extract_element 1 rmap)
        (return Nothing)
    return $ vl1_patch name elt1 maybe_elt2
    where
    get :: Sysex.RecordVal a => String -> Either String a
    get = flip Sysex.get_rmap rmap

vl1_patch :: InstTypes.Name -> ElementInfo -> Maybe ElementInfo
    -> MidiInst.Patch
vl1_patch name elt1 maybe_elt2 =
    (if is_pressure then MidiInst.pressure else id) $
        MidiInst.common#Common.tags #= map ((,) "vl1-element") names $
        MidiInst.named_patch pb_range name cmap
    where
    (pb_ranges, names, cc_groups) = unzip3 $ elt1 : Maybe.maybeToList maybe_elt2
    -- If it has a pressure control, then assume it's a breath patch.
    is_pressure = CC.breath `elem` map fst cmap

    -- Optimistically take the widest range.
    Just pb_range = Seq.maximum_on (\(low, high) -> max (abs low) (abs high))
        pb_ranges
    cmap = Map.toList $ Map.mapMaybe highest_prio $
        Map.unionsWith (++) (map Map.fromList cc_groups)
    highest_prio cs = List.find (`elem` cs)
        (map (ScoreT.unchecked_control . fst) vl1_control_map)

extract_element :: Int -> Sysex.RMap -> Either String ElementInfo
extract_element n rmap = do
    controls <- forM vl1_control_map $ \(name, has_upper_lower) -> do
        cc <- get ["control", name, "control"]
        depths <- if has_upper_lower
            then do
                upper <- get ["control", name, "upper depth"]
                lower <- get ["control", name, "lower depth"]
                return [upper, lower]
            else (:[]) <$> get ["control", name, "depth"]
        return (clean name, cc, depths)
    pb_up <- get ["control", "pitch", "upper depth"]
    pb_down <- get ["control", "pitch", "lower depth"]
    name <- get ["name"]
    return ((pb_up, pb_down), name, process_controls controls)
    where
    get :: (Sysex.RecordVal a) => [Text] -> Either String a
    get k = Sysex.get_rmap
        (untxt (Text.intercalate "." (["element", showt n] ++ k))) rmap
    -- The vl1 mostly uses the midi control list, except sticks some
    -- internal ones in there.  TODO 120 is aftertouch.
    valid_control cc = cc>0 && (cc<11 || cc>15) && cc<120
    clean = Text.map $ \c -> if c == ' ' then '-' else c

    process_controls :: [(Text, Midi.Control, [Word8])]
        -> [(Midi.Control, [ScoreT.Control])]
    process_controls controls =
        [(cc, map snd grp) | (cc, grp) <- Seq.keyed_group_sort fst by_cc]
        where
        by_cc =
            [ (cc, ScoreT.unchecked_control name)
            | (name, cc, depths) <- controls, valid_control cc
            , maximum (map abs depths) >= 32
            ]

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
vl1_control_map :: [(Text, Bool)]
vl1_control_map =
    [ ("embouchure", True)
    , ("pressure", False)
    , ("amplitude", False)
    , ("scream", False)
    , ("growl", False)
    , ("vibrato", False)
    , ("dynamic filter", False)
    , ("throat formant", False)
    , ("breath noise", False)
    , ("harmonic enhancer", False)
    , ("tonguing", False)
    , ("damping", False)
    , ("absorption", False)
    ]
