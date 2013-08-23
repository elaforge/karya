-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
-- | Yamaha VL1 synthesizer.
module Local.Instrument.Vl1 where
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Word (Word8)

import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified Text.Printf as Printf

import Util.Control
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Midi.CC as CC
import qualified Midi.Midi as Midi
import qualified Midi.Encode

import qualified Derive.Score as Score
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Sysex as Sysex
import Local.Instrument.Vl1Spec
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
    let dirs = map ((dir </> synth_name) </>)
            ["vc", "sysex", "patchman1", "patchman2"]
    patches <- concatMapM parse_dir dirs
    builtins <- parse_builtins (dir </> synth_name </> builtin)
    MidiInst.save_patches synth (builtins ++ patches) synth_name dir

synth :: Instrument.Synth
synth = Instrument.synth (txt synth_name) "Yamaha VL1" []

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
syx_fname num name = Printf.printf "%03d.%s" num
    (untxt $ MidiDb.clean_inst_name name)

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

modify :: (Monad m) => [a -> m a] -> a -> m a
modify = foldr (<=<) return

put_int :: String -> Int -> Sysex.RMap -> Either String Sysex.RMap
put_int path int = Sysex.put_rmap path int

parse_builtins :: FilePath -> IO [Instrument.Patch]
parse_builtins fn = do
    results <- parse_file fn
    mapM_ Log.warn (Either.lefts results)
    return [Sysex.initialize_program 0 i patch
        | (i, Right patch) <- zip [0..] results]

parse_dir :: FilePath -> IO [Instrument.Patch]
parse_dir dir = do
    fns <- File.listRecursive (const True) dir
    (warns, patches) <- Seq.partition_either . concat <$> mapM parse_file fns
    mapM_ Log.warn warns
    return patches

parse_file :: FilePath -> IO [Either String Instrument.Patch]
parse_file fn = do
    syxs <- file_to_syx fn
    txt <- fromMaybe "" <$> File.ignoreEnoent
        (Text.IO.readFile (FilePath.replaceExtension fn ".txt"))
    let results = map (record_to_patch <=< decode_sysex) syxs
    return [either (Left . failed i) (Right . combine fn txt syx) result
        | (i, syx, result) <- zip3 [1..] syxs results]
    where
    failed i msg = "parsing " ++ show fn ++ "/" ++ show i ++ ": " ++ msg

combine :: FilePath -> Text -> ByteString -> Instrument.Patch
    -> Instrument.Patch
combine fn txt syx patch = Sysex.add_file fn $ patch
    { Instrument.patch_text = Text.strip txt
    , Instrument.patch_initialize =
        Instrument.InitializeMidi [Midi.Encode.decode syx]
    }

decode_sysex :: ByteString -> Either String Sysex.RMap
decode_sysex bytes = fst <$> decode patch_spec bytes

encode_sysex :: Sysex.RMap -> Either String ByteString
encode_sysex = fmap append_suffix . encode patch_spec

file_to_syx :: FilePath -> IO [ByteString]
file_to_syx fn = map add_extra_zero <$> case FilePath.takeExtension fn of
        ".all" -> split_1bk Nothing <$> B.readFile fn
        ".1vc" -> split_1vc <$> B.readFile fn
        ".1bk" -> split_1bk Nothing <$> B.readFile fn
        ".syx" -> split_syx <$> B.readFile fn
        ".txt" -> return []
        ".rec" -> return []
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

-- | For some reason, some sysexes come out with a 0 for device numbers, and
-- some omit it entirely.
add_extra_zero :: ByteString -> ByteString
add_extra_zero bytes
    | B.isPrefixOf short bytes = long <> B.drop (B.length short) bytes
    | otherwise = bytes
    where
    long = B.pack [0xf0, Midi.yamaha_code, 0, 0x7a]
    short = B.pack [0xf0, Midi.yamaha_code, 0x7a]

drop_extra_zero :: ByteString -> ByteString
drop_extra_zero bytes
    | B.isPrefixOf long bytes = short <> B.drop (B.length long) bytes
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
type ElementInfo = (Control.PbRange, Text, [(Midi.Control, [Score.Control])])

record_to_patch :: Sysex.RMap -> Either String Instrument.Patch
record_to_patch rmap = do
    name <- get "name"
    elt1 <- extract_element 0 rmap
    maybe_elt2 <- ifM ((== ("dual" :: Text)) <$> get "voice mode")
        (Just <$> extract_element 1 rmap)
        (return Nothing)
    vl1_patch name elt1 maybe_elt2
    where
    get :: (Sysex.RecordVal a) => String -> Either String a
    get = flip Sysex.get_rmap rmap

vl1_patch :: Instrument.InstrumentName -> ElementInfo
    -> Maybe ElementInfo -> Either String Instrument.Patch
vl1_patch name elt1 maybe_elt2 =
    return $ (if is_pressure then MidiInst.pressure else id)
        (Instrument.patch inst)
            { Instrument.patch_tags = map ((,) "vl1-element") names }
    where
    inst = Instrument.instrument name cmap pb_range
    (pb_ranges, names, cc_groups) =
        unzip3 $ elt1 : Maybe.maybeToList maybe_elt2
    -- If it has a pressure control, then assume it's a breath patch.
    is_pressure = CC.breath `elem` map fst cmap

    -- Optimistically take the widest range.
    Just pb_range = Seq.maximum_on (\(low, high) -> max (abs low) (abs high))
        pb_ranges
    cmap = Map.toList $ Map.mapMaybe highest_prio $
        Map.unionsWith (++) (map Map.fromList cc_groups)
    highest_prio cs = List.find (`elem` cs)
        (map (Score.control . fst) vl1_control_map)

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
        -> [(Midi.Control, [Score.Control])]
    process_controls controls =
        [(cc, map snd grp) | (cc, grp) <- Seq.keyed_group_on fst by_cc]
        where
        by_cc =
            [ (cc, Score.control name)
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
