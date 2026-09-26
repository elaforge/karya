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
import qualified Util.Exceptions as Exceptions
import qualified Util.Files as Files
import qualified Util.Log as Log
import qualified Util.Lists as Lists

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Instrument.MidiInstDb as MidiInstDb
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Instrument.Sysex as Sysex

import qualified Midi.CC as CC
import qualified Midi.Encode
import qualified Midi.Midi as Midi

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import qualified User.Elaforge.Instrument.Vl1Spec as Vl1Spec

import           Global


synthName :: InstT.SynthName
synthName = "vl1"

load :: Path.AppDir -> IO (Maybe MidiInst.Synth)
load = MidiInstDb.loadSynth (const mempty) synthName "Yamaha Vl1"

-- | Read the patch file, scan the sysex dir, and save the results in a cache.
makeDb :: Path.AppDir -> IO ()
makeDb appDir = do
    let dir = Path.toAbsolute appDir Config.instrumentDir
            </> untxt synthName
    let dirs = map (dir</>) ["vc", "sysex", "patchman1", "patchman2"]
    patches <- concatMapM parseDir dirs
    builtins <- parseBuiltins (dir </> builtin)
    MidiInstDb.saveSynth appDir synthName (builtins ++ patches)

builtin :: FilePath
builtin = "vl1v2-factory/vl1_ver2.all"


-- * parse

-- | Write .syx and .rec files for the contents of the given file.
extractSyxs :: FilePath -> FilePath -> IO ()
extractSyxs dir fn = do
    syxs <- fileToSyx fn
    forM_ (zip [0..] syxs) $ \(n, syx) -> do
        let Right rec = decodeSysex syx
            Right name = Sysex.getRmap "name" rec
            fn = dir </> syxFname n name
        B.writeFile (fn ++ ".syx") syx
        writeFile (fn ++ ".rec") (unlines (Sysex.showFlat rec))

syxFname :: Int -> Text -> FilePath
syxFname num name =
    Printf.printf "%03d.%s" num (untxt $ MidiInstDb.cleanName name)

sendToBuffer = modify
    [ putInt "memory type" 0x7f
    , putInt "memory number" 0
    ]

sendToPatch num = modify
    [ putInt "memory type" 0
    , putInt "memory number" num
    ]

setPitchBend = modify
    [ putInt "element.0.control.pitch.lower depth" (-12)
    , putInt "element.0.control.pitch.upper depth" 12
    , putInt "element.1.control.pitch.lower depth" (-12)
    , putInt "element.1.control.pitch.upper depth" 12
    ]

modify :: Monad m => [a -> m a] -> a -> m a
modify = foldr (<=<) pure

putInt :: String -> Int -> Sysex.RMap -> Either String Sysex.RMap
putInt path int = Sysex.putRmap path int

parseBuiltins :: FilePath -> IO [MidiInst.Patch]
parseBuiltins fn = do
    (warns, patches) <- Either.partitionEithers <$> parseFile fn
    mapM_ (Log.warn . txt) warns
    pure $ zipWith initialize [0..] patches
    where
    initialize n = MidiInst.patch#Patch.initialize
        #= Patch.initializeMidi (Midi.programChange 0 n)

parseDir :: FilePath -> IO [MidiInst.Patch]
parseDir dir = do
    fns <- Files.listRecursive (const True) dir
    (warns, patches) <- Either.partitionEithers . concat <$> mapM parseFile fns
    mapM_ (Log.warn . txt) warns
    pure patches

parseFile :: FilePath -> IO [Either String MidiInst.Patch]
parseFile fn = do
    syxs <- fileToSyx fn
    doc <- fromMaybe "" <$> Exceptions.ignoreEnoent
        (Text.IO.readFile (FilePath.replaceExtension fn ".txt"))
    let results = map (recordToPatch <=< decodeSysex) syxs
    pure
        [ bimap (failed i) (combine fn doc syx) result
        | (i, syx, result) <- zip3 [1..] syxs results
        ]
    where
    failed i msg = "parsing " ++ show fn ++ "/" ++ show i ++ ": " ++ msg

combine :: FilePath -> Text -> ByteString -> MidiInst.Patch -> MidiInst.Patch
combine fn doc syx =
    (MidiInst.common %= Sysex.addFile fn)
    . (MidiInst.doc #= Doc.Doc (Text.strip doc))
    . (MidiInst.patch#Patch.initialize #=
        Patch.InitializeMidi [Midi.Encode.decode syx])

decodeSysex :: ByteString -> Either String Sysex.RMap
decodeSysex bytes = fst <$> Vl1Spec.decode Vl1Spec.patchSpec bytes

encodeSysex :: Sysex.RMap -> Either String ByteString
encodeSysex = fmap appendSuffix . Vl1Spec.encode Vl1Spec.patchSpec

fileToSyx :: FilePath -> IO [ByteString]
fileToSyx fn = map addExtraZero <$> case FilePath.takeExtension fn of
    ".all" -> split1bk Nothing <$> B.readFile fn
    ".1vc" -> split1vc <$> B.readFile fn
    ".1bk" -> split1bk Nothing <$> B.readFile fn
    ".syx" -> splitSyx <$> B.readFile fn
    ".txt" -> pure []
    ".rec" -> pure []
    _ -> Log.warn ("skipping " <> showt fn) >> pure []
    where
    -- | Convert .1vc format to .syx format.  Derived by looking at vlone70
    -- conversions with od.
    split1vc bytes = [bytesToSyx Nothing (B.drop 0xc00 bytes)]
    splitSyx = map (<> B.singleton Midi.Encode.eoxByte)
        . filter (not . B.null) . B.split Midi.Encode.eoxByte

split1bk :: Maybe Word8 -> ByteString -> [ByteString]
split1bk memory = zipWith (\n -> bytesToSyx ((+n) <$> memory)) [0..] . split
    where
    split bytes = takeWhile (not . B.all (==0) . B.take 20) $
        map (flip B.drop bytes) offsets
    offsets = [0xc00, 0x1800..]

-- | For some reason, some sysexes come out with a 0 for device numbers, and
-- some omit it entirely.
addExtraZero :: ByteString -> ByteString
addExtraZero bytes
    | B.isPrefixOf short bytes = long <> B.drop (B.length short) bytes
    | otherwise = bytes
    where
    long = B.pack [0xf0, Midi.yamahaCode, 0, 0x7a]
    short = B.pack [0xf0, Midi.yamahaCode, 0x7a]

-- | Wrap sysex codes around the raw bytes.
bytesToSyx :: Maybe Word8 -> ByteString -> ByteString
bytesToSyx memory bytes = appendSuffix $
    Vl1Spec.vl1Header (2 + 14 + size)
        -- memory type, memory number
        <> B.pack (maybe [0x7f, 0] (\n -> [0, n]) memory)
        <> B.replicate 14 0 -- padding
        <> B.take size bytes
    where size = 0xc1c - 0x20

appendSuffix :: ByteString -> ByteString
appendSuffix bytes = bytes <> B.pack [checksum, Midi.Encode.eoxByte]
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

recordToPatch :: Sysex.RMap -> Either String MidiInst.Patch
recordToPatch rmap = do
    name <- get "name"
    elt1 <- extractElement 0 rmap
    maybeElt2 <- ifM ((== ("dual" :: Text)) <$> get "voice mode")
        (Just <$> extractElement 1 rmap)
        (pure Nothing)
    pure $ vl1Patch name elt1 maybeElt2
    where
    get :: Sysex.RecordVal a => String -> Either String a
    get = flip Sysex.getRmap rmap

vl1Patch :: InstT.Name -> ElementInfo -> Maybe ElementInfo -> MidiInst.Patch
vl1Patch name elt1 maybeElt2 =
    (if isPressure then MidiInst.pressure else id) $
        MidiInst.common#Common.tags #= map ((,) "vl1-element") names $
        MidiInst.namedPatch pbRange name cmap
    where
    (pbRanges, names, ccGroups) = unzip3 $ elt1 : Maybe.maybeToList maybeElt2
    -- If it has a pressure control, then assume it's a breath patch.
    isPressure = CC.breath `elem` map fst cmap

    -- Optimistically take the widest range.
    Just pbRange = Lists.maximumOn (\(low, high) -> max (abs low) (abs high))
        pbRanges
    cmap = Map.toList $ Map.mapMaybe highestPrio $
        Map.unionsWith (++) (map Map.fromList ccGroups)
    highestPrio cs = List.find (`elem` cs)
        (map (ScoreT.Control . fst) vl1ControlMap)

extractElement :: Int -> Sysex.RMap -> Either String ElementInfo
extractElement n rmap = do
    controls <- forM vl1ControlMap $ \(name, hasUpperLower) -> do
        cc <- get ["control", name, "control"]
        depths <- if hasUpperLower
            then do
                upper <- get ["control", name, "upper depth"]
                lower <- get ["control", name, "lower depth"]
                pure [upper, lower]
            else (:[]) <$> get ["control", name, "depth"]
        pure (clean name, cc, depths)
    pbUp <- get ["control", "pitch", "upper depth"]
    pbDown <- get ["control", "pitch", "lower depth"]
    name <- get ["name"]
    pure ((pbUp, pbDown), name, processControls controls)
    where
    get :: (Sysex.RecordVal a) => [Text] -> Either String a
    get k = Sysex.getRmap
        (untxt (Text.intercalate "." (["element", showt n] ++ k))) rmap
    -- The vl1 mostly uses the midi control list, except sticks some
    -- internal ones in there.  TODO 120 is aftertouch.
    validControl cc = cc>0 && (cc<11 || cc>15) && cc<120
    clean = Text.map $ \c -> if c == ' ' then '-' else c

    processControls :: [(Text, Midi.Control, [Word8])]
        -> [(Midi.Control, [ScoreT.Control])]
    processControls controls =
        [(cc, map snd grp) | (cc, grp) <- Lists.keyedGroupSort fst byCc]
        where
        byCc =
            [ (cc, ScoreT.Control name)
            | (name, cc, depths) <- controls, validControl cc
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
vl1ControlMap :: [(Text, Bool)]
vl1ControlMap =
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
