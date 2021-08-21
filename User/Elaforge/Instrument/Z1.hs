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


synth_name :: InstT.SynthName
synth_name = "z1"

load :: Path.AppDir -> IO (Maybe MidiInst.Synth)
load = MidiInstDb.load_synth (const mempty) synth_name "Korg Z1"

make_db :: Path.AppDir -> IO ()
make_db app_dir = do
    let dir = Path.to_absolute app_dir Config.instrument_dir
            </> untxt synth_name
    bank_a <- Sysex.parse_builtins 0 program_dump (dir </> "bank_a.syx")
    bank_b <- Sysex.parse_builtins 1 program_dump (dir </> "bank_b.syx")
    sysex <- Sysex.parse_dir [current_program_dump, program_dump, sysex_manager]
        (dir </> "sysex")
    MidiInstDb.save_synth app_dir synth_name $
        map (override_pb . MidiInst.patch_from_pair) $
        concat [bank_a, bank_b, sysex]
    where
    current_program_dump =
        fmap (:[]) . (rmap_to_patch <=< decode_current_program)
    program_dump = mapM rmap_to_patch <=< decode_program_dump
    -- Each patch has its own pb range, but you can override them in the
    -- multiset.
    override_pb = MidiInst.patch#Patch.defaults#Patch.pitch_bend_range
        #= Just (-24, 24)

synth_controls :: [(Midi.Control, ScoreT.Control)]
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

-- | Decode a dump for a program at a certain memory location.  This also
-- parses bank dumps, which are just encoded as a bunch of dumps at consecutive
-- memory locations.
decode_program_dump :: ByteString -> Either String [Sysex.RMap]
decode_program_dump bytes = do
    -- If there is just one, then the bank and unit fields are valid.
    -- Otherwise, they are 0.
    (rmap, bytes) <- decode program_dump_header bytes
    let syxs = exact_chunks
            (spec_bytes patch_spec) (dekorg bytes)
    mapM (fmap ((rmap <>) . fst) . decode patch_spec) syxs

sysex_manager :: ByteString -> Either String [(Patch.Patch, Common.Common ())]
sysex_manager bytes = do
    bytes <- Sysex.expect_bytes bytes $ Char8.pack "Sysex Manager"
    -- The first sysex is something else.
    let sysexes = drop 1 $ Sysex.extract_sysex bytes
    patches <- mapM (rmap_to_patch <=< decode_current_program) sysexes
    -- Add the initialize here, since 'bytes' isn't actually a valid sysex.
    return [(Sysex.initialize_sysex sysex patch, common)
        | (sysex, (patch, common)) <- zip sysexes patches]

test_decode = do
    -- let fn = "inst_db/z1/sysex/lib2/apollo44.syx"
    -- let fn = "inst_db/z1/sysex/lib1/z1 o00o00 Syncapacitor.syx"
    -- let fn = "inst_db/z1/sysex/lib1/z1 o00o05 Composite Synth.syx"
    let fn = "inst_db/z1/sysex/lib1/z1 o00o00 .C.H.A.O.S..syx"
    decode_current_program <$> B.readFile fn

-- * encode sysex

-- set_pitch_bend fn = do
--     bytes <- B.readFile fn
--     records <- require "parse" $ decode_program_dump bytes
--     records <- require "set" $ mapM set records

set_bank_pitch_bend :: Bank -> FilePath -> IO ()
set_bank_pitch_bend bank fn = do
    bytes <- B.readFile fn
    records <- require "parse" $ decode_program_dump bytes
    records <- require "set" $ mapM set records
    bytes <- require "unparse" $ encode_bank_dump All bank records
    B.writeFile (fn ++ ".modified") bytes
    where
    set = Sysex.put_rmap "pitch bend.intensity +" (24 :: Int)
        <=< Sysex.put_rmap "pitch bend.intensity -" (-24 :: Int)
    require msg = either (errorIO . ((msg <> ": ") <>) . txt) return

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
        (concatMapM (encode patch_spec) rmaps)
    where
    set_bank = Sysex.put_rmap "bank" (Text.toLower (showt bank))
        <=< Sysex.put_rmap "unit" (Text.toLower (showt unit))

encode_sysex :: Either String ByteString -> Either String ByteString
    -> Either String ByteString
encode_sysex encode_header encode_body = do
    header <- encode_header
    body <- encode_body
    return $ header <> enkorg body <> B.singleton Midi.Encode.eox_byte

-- ** record

rmap_to_patch :: Sysex.RMap -> Either String (Patch.Patch, Common.Common ())
rmap_to_patch rmap = do
    name <- get "name"
    category <- get "category"
    pb_range <- (,) <$> get "pitch bend.intensity -"
        <*> get "pitch bend.intensity +"
    osc1 <- get "osc.0.type"
    osc2 <- get "osc.1.type"
    let tags = [("category", category), ("z1-osc", osc1), ("z1-osc", osc2)]
    let common = Common.tags #= tags $ Common.common ()
    return (Patch.patch pb_range name, common)
    where
    get :: (Sysex.RecordVal a) => String -> Either String a
    get = flip Sysex.get_rmap rmap

current_multi_data_dump :: Word8
current_multi_data_dump = 0x69

multi_data_dump :: Word8
multi_data_dump = 0x4d

-- | Z1 sysexes use a scheme where the eighth bits are packed into a single
-- byte preceeding its 7 7bit bytes.
dekorg :: ByteString -> ByteString
dekorg = mconcatMap smoosh . chunks 8
    where
    smoosh bs = case B.uncons bs of
        Just (b7, bytes) -> snd $
            B.mapAccumL (\i to -> (i+1, copy_bit b7 i to)) 0 bytes
        Nothing -> mempty
    copy_bit from i to = if Bits.testBit from i
        then Bits.setBit to 7 else Bits.clearBit to 7

enkorg :: ByteString -> ByteString
enkorg = mconcatMap expand . chunks 7
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

-- * test

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
