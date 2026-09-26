-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Korg Z1 keyboard.
module User.Elaforge.Instrument.Z1 where
import qualified Data.Bits as Bits
import           Data.Bits ((.|.))
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text
import           Data.Word (Word8)

import           System.FilePath ((</>))

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Instrument.MidiInstDb as MidiInstDb
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Instrument.Sysex as Sysex

import qualified Midi.Encode
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch

import           Global
import           User.Elaforge.Instrument.Z1Spec


synthName :: InstT.SynthName
synthName = "z1"

load :: Path.AppDir -> IO (Maybe MidiInst.Synth)
load = MidiInstDb.loadSynth (const mempty) synthName "Korg Z1"

makeDb :: Path.AppDir -> IO ()
makeDb appDir = do
    let dir = Path.toAbsolute appDir Config.instrumentDir
            </> untxt synthName
    bankA <- Sysex.parseBuiltins 0 programDump (dir </> "bank_a.syx")
    bankB <- Sysex.parseBuiltins 1 programDump (dir </> "bank_b.syx")
    sysex <- Sysex.parseDir [currentProgramDump, programDump, sysexManager]
        (dir </> "sysex")
    MidiInstDb.saveSynth appDir synthName $
        map (overridePb . MidiInst.patchFromPair) $
        concat [bankA, bankB, sysex]
    where
    currentProgramDump =
        fmap (:[]) . (rmapToPatch <=< decodeCurrentProgram)
    programDump = mapM rmapToPatch <=< decodeProgramDump
    -- Each patch has its own pb range, but you can override them in the
    -- multiset.
    overridePb = MidiInst.patch#Patch.defaults#Patch.pitchBendRange
        #= Just (-24, 24)

synthControls :: [(Midi.Control, ScoreT.Control)]
synthControls =
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

decodeCurrentProgram :: ByteString -> Either String Sysex.RMap
decodeCurrentProgram bytes = do
    (header, bytes) <- decode currentProgramDumpHeader bytes
    (rmap, _) <- decode patchSpec (dekorg bytes)
    pure $ header <> rmap

-- | Decode a dump for a program at a certain memory location.  This also
-- parses bank dumps, which are just encoded as a bunch of dumps at consecutive
-- memory locations.
decodeProgramDump :: ByteString -> Either String [Sysex.RMap]
decodeProgramDump bytes = do
    -- If there is just one, then the bank and unit fields are valid.
    -- Otherwise, they are 0.
    (rmap, bytes) <- decode programDumpHeader bytes
    let syxs = exactChunks
            (specBytes patchSpec) (dekorg bytes)
    mapM (fmap ((rmap <>) . fst) . decode patchSpec) syxs

sysexManager :: ByteString -> Either String [(Patch.Patch, Common.Common ())]
sysexManager bytes = do
    bytes <- Sysex.expectBytes bytes $ Char8.pack "Sysex Manager"
    -- The first sysex is something else.
    let sysexes = drop 1 $ Sysex.extractSysex bytes
    patches <- mapM (rmapToPatch <=< decodeCurrentProgram) sysexes
    -- Add the initialize here, since 'bytes' isn't actually a valid sysex.
    pure [(Sysex.initializeSysex sysex patch, common)
        | (sysex, (patch, common)) <- zip sysexes patches]

test_decode = do
    -- let fn = "inst_db/z1/sysex/lib2/apollo44.syx"
    -- let fn = "inst_db/z1/sysex/lib1/z1 o00o00 Syncapacitor.syx"
    -- let fn = "inst_db/z1/sysex/lib1/z1 o00o05 Composite Synth.syx"
    let fn = "inst_db/z1/sysex/lib1/z1 o00o00 .C.H.A.O.S..syx"
    decodeCurrentProgram <$> B.readFile fn

-- * encode sysex

-- set_pitch_bend fn = do
--     bytes <- B.readFile fn
--     records <- require "parse" $ decode_program_dump bytes
--     records <- require "set" $ mapM set records

setBankPitchBend :: Bank -> FilePath -> IO ()
setBankPitchBend bank fn = do
    bytes <- B.readFile fn
    records <- require "parse" $ decodeProgramDump bytes
    records <- require "set" $ mapM set records
    bytes <- require "unparse" $ encodeBankDump All bank records
    B.writeFile (fn ++ ".modified") bytes
    where
    set = Sysex.putRmap "pitch bend.intensity +" (24 :: Int)
        <=< Sysex.putRmap "pitch bend.intensity -" (-24 :: Int)
    require msg = either (errorIO . ((msg <> ": ") <>) . txt) pure

encodeCurrentProgram :: Sysex.RMap -> Either String ByteString
encodeCurrentProgram rmap =
    encodeSysex (encode currentProgramDumpHeader rmap)
        (encode patchSpec rmap)

encodeProgramDump :: Sysex.RMap -> Either String ByteString
encodeProgramDump rmap =
    encodeSysex (encode programDumpHeader rmap)
        (encode patchSpec rmap)

data Unit = Program | Bank | All deriving (Show)
data Bank = A | B deriving (Show)

encodeBankDump :: Unit -> Bank -> [Sysex.RMap] -> Either String ByteString
encodeBankDump unit bank rmaps = do
    headerRmap <- setBank $ Sysex.specToRmap programDumpHeader
    encodeSysex (encode programDumpHeader headerRmap)
        (concatMapM (encode patchSpec) rmaps)
    where
    setBank = Sysex.putRmap "bank" (Text.toLower (showt bank))
        <=< Sysex.putRmap "unit" (Text.toLower (showt unit))

encodeSysex :: Either String ByteString -> Either String ByteString
    -> Either String ByteString
encodeSysex encodeHeader encodeBody = do
    header <- encodeHeader
    body <- encodeBody
    pure $ header <> enkorg body <> B.singleton Midi.Encode.eoxByte

-- ** record

rmapToPatch :: Sysex.RMap -> Either String (Patch.Patch, Common.Common ())
rmapToPatch rmap = do
    name <- get "name"
    category <- get "category"
    pbRange <- (,) <$> get "pitch bend.intensity -"
        <*> get "pitch bend.intensity +"
    osc1 <- get "osc.0.type"
    osc2 <- get "osc.1.type"
    let tags = [("category", category), ("z1-osc", osc1), ("z1-osc", osc2)]
    let common = Common.tags #= tags $ Common.common ()
    pure (Patch.patch pbRange name, common)
    where
    get :: (Sysex.RecordVal a) => String -> Either String a
    get = flip Sysex.getRmap rmap

currentMultiDataDump :: Word8
currentMultiDataDump = 0x69

multiDataDump :: Word8
multiDataDump = 0x4d

-- | Z1 sysexes use a scheme where the eighth bits are packed into a single
-- byte preceeding its 7 7bit bytes.
dekorg :: ByteString -> ByteString
dekorg = mconcatMap smoosh . chunks 8
    where
    smoosh bs = case B.uncons bs of
        Just (b7, bytes) -> snd $
            B.mapAccumL (\i to -> (i+1, copyBit b7 i to)) 0 bytes
        Nothing -> mempty
    copyBit from i to = if Bits.testBit from i
        then Bits.setBit to 7 else Bits.clearBit to 7

enkorg :: ByteString -> ByteString
enkorg = mconcatMap expand . chunks 7
    where
    expand bs = B.cons bits (B.map (`Bits.clearBit` 7) bs)
        where bits = B.foldr getBits 0 bs
    getBits b accum =
        Bits.shiftL accum 1 .|. (if Bits.testBit b 7 then 1 else 0)

chunks :: Int -> ByteString -> [ByteString]
chunks size bs
    | B.null pre = []
    | otherwise = pre : chunks size post
    where (pre, post) = B.splitAt size bs

exactChunks :: Int -> ByteString -> [ByteString]
exactChunks size bs
    | B.length pre < size = []
    | otherwise = pre : exactChunks size post
    where (pre, post) = B.splitAt size bs

-- * test

test_multiset = do
    bytes <- B.drop 9 <$> B.readFile "inst_db/multi1.syx"
    pure $ decode multisetSpec (dekorg bytes)

test_dump = do
    bytes <- B.readFile "inst_db/z1/bank_b.syx"
    pure $ decodeProgramDump bytes

test_encode = do
    bytes <- B.readFile "inst_db/z1/bank_b.syx"
    let Right recs = decodeProgramDump bytes
    pure $ encode patchSpec (head recs)

test_patch = do
    bytes <- B.readFile
        "inst_db/z1/sysex/lib1/z1 o00o00 ANALOG INIT.syx"
    pure $ decodeCurrentProgram bytes

readPatch = do
    b <- dekorg . B.drop 6 <$> B.readFile
        "inst_db/z1/sysex/lib1/z1 o00o00 ANALOG INIT.syx"
    pure $ decode patchSpec b
