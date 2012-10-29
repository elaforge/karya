-- | Korg Z1 keyboard.
module Local.Instrument.Z1 where
import qualified Data.Bits as Bits
import qualified Data.List as List
import Data.Word (Word8)

import System.FilePath ((</>))

import Util.Control
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Parse as Parse
import qualified App.MidiInst as MidiInst


name :: FilePath
name = "z1"

load :: FilePath -> IO [MidiInst.SynthDesc]
load = MidiInst.load_db (const MidiInst.empty_code) name

make_db :: FilePath -> IO ()
make_db dir = do
    patches <- (++) <$>
        Parse.patch_file (dir </> name)
        <*> Parse.parse_sysex_dir korg_sysex (dir </> "z1_sysex")
    MidiInst.save_patches synth patches name dir

synth :: Instrument.Synth
synth = Instrument.synth name synth_controls

synth_controls :: [(Midi.Control, String)]
synth_controls =
    -- The PE controls are the "performance expression" knobs whose effect
    -- depends on the instrument.
    [ (13, "pe1"), (20, "pe2"), (21, "pe3"), (22, "pe4"), (23, "pe5")
    , (16, "pad-x"), (17, "pad-y")
    , (65, "z1-port-sw") -- Turn portamento on and off.
    , (80, "z1-sw-1"), (81, "z1-sw-2") -- General purpose on/off switches.
    -- Various filter cutoff etc.
    ]

-- * parse sysex

-- TODO some of the dumps are program data dump format which starts
-- f0 42 30 46 - 4c <ub> <pp> 00
-- instead of
-- f0 42 30 46 - 40 01
--
-- I should write something to convert them into current program format.

-- Interactive testing.
-- tparse = Parse.parse_sysex_dir korg_sysex "Local/Instrument/z1_sysex"
-- tshow ps = mapM_ putStrLn (map Instrument.patch_summary ps)

korg_sysex :: Parse.ByteParser Instrument.Patch
korg_sysex = do
    Parse.start_sysex Midi.korg_code
    Parse.match_bytes [0x30, 0x46]
    patch <- current_program_dump
    Parse.end_sysex
    return patch

current_program_dump :: Parse.ByteParser Instrument.Patch
current_program_dump = do
    Parse.match_bytes [0x40, 0x01]
    contents <- fmap dekorgify Parse.to_eox
    return $ korg_patch contents

korg_patch :: [Word8] -> Instrument.Patch
korg_patch bytes = make_patch (name, category, (pb_up, pb_down), osc1, osc2)
    where
    -- These come from the sysex spec MIDIImp_z1 PROG_PRM.TXT from korg.
    common_off = 25 + 19*4 + 11*4
    osc1_off = common_off + 9
    osc2_off = osc1_off + 52
    -- The doc says [Intensity(+), Intensity(-)] but that seems to
    -- mean [down, up].
    [cat, u_pb_down, u_pb_up, osc1_type, osc2_type] =
        map (bytes!!) [16, common_off, common_off+1, osc1_off, osc2_off]
    pb_down = to_signed u_pb_down
    pb_up = to_signed u_pb_up
    name = Seq.strip $ Parse.to_string (take 16 bytes)
    category = Seq.at categories cat
    osc1 = Seq.at osc_types osc1_type
    osc2 = Seq.at osc_types osc2_type

-- | Convert an 8 bit 2s complement word to a signed integer.
to_signed :: Word8 -> Integer
to_signed b
    | Bits.testBit b 7 = negate $ fromIntegral $ Bits.complement b + 1
    | otherwise = fromIntegral b

make_patch ::
    (String, Maybe String, Control.PbRange, Maybe String, Maybe String)
    -> Instrument.Patch
make_patch (name, cat, pb_range, osc1, osc2) =
    -- Initialization will be filled in later.
    (Instrument.patch inst) { Instrument.patch_tags = tags }
    where
    inst = Instrument.instrument name [] pb_range
    tags = maybe_tags
        [("z1-category", cat), ("z1-osc", osc1), ("z1-osc", osc2)]
    maybe_tags tags = [(k, v) | (k, Just v) <- tags]

-- | The Z1 has a built-in set of categories. Map the category index to the
-- name.
categories :: [String]
categories =
    [ "Synth-Hard", "Synth-Soft", "Synth-Lead", "Synth-Motion", "Synth-Bass"
    , "E.Piano", "Organ", "Keyboard", "Bell", "Strings", "Band/Choir"
    , "Brass", "Reed/Wind", "Guitar/Plucked", "Bass", "Percussive"
    , "Argpeggio", "SFX/Other"
    ]

osc_types :: [String]
osc_types =
    [ "standard", "comb", "vpm", "resonance", "ring-mod", "cross-mod", "sync"
    , "organ", "electric-piano", "brass", "reed", "plucked", "bowed"
    ]

-- | Z1 sysexes use a scheme where the eighth bits are packed into a single
-- byte preceeding its 7 7bit bytes.
dekorgify :: [Word8] -> [Word8]
dekorgify [] = []
dekorgify (b7:bytes) =
    [copy_bit b7 i b | (i, b) <- zip [0..] b7group] ++ dekorgify rest
    where
    (b7group, rest) = List.splitAt 7 bytes
    copy_bit from i to = if Bits.testBit from i
        then Bits.setBit to 7 else Bits.clearBit to 7
