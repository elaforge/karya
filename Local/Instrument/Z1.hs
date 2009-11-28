{- | Instrument db for the Korg Z1 keyboard.
-}
module Local.Instrument.Z1 where
import qualified Data.Bits as Bits
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word
import System.FilePath ((</>))

import qualified Util.Seq as Seq

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Control as Control
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Parse as Parse


load dir = Parse.patch_file (Instrument.synth_name z1) (dir </> "z1")
    >>= MidiDb.load_synth_desc z1
load_slow dir = Parse.parse_sysex_dir korg_sysex (dir </> "z1_sysex")
    >>= MidiDb.load_synth_desc z1

z1 = Instrument.synth "z1" "z1" z1_controls

z1_controls =
    [
    -- The PE controls are the "performance expression" knobs whose effect
    -- depends on the instrument.
    (13, "pe 1"), (20, "pe 2"), (21, "pe 3"), (22, "pe 4"), (23, "pe 5")
    , (16, "pad x"), (17, "pad y")
    , (65, "z1 port sw")
    -- General purpose on/off switches.
    , (80, "z1 sw 1"), (81, "z1 sw 2")

    -- Various filter cutoff etc.
    ]

-- * parse sysex

-- TODO some of the dumps are program data dump format which starts
-- f0 42 30 46 - 4c <ub> <pp> 00
-- instead of
-- f0 42 30 46 - 40 01
--
-- I should write something to convert them into current program format.

tparse = Parse.parse_sysex_dir korg_sysex "Local/Instrument/z1_sysex"
tshow ps = mapM_ putStrLn (map Instrument.patch_summary ps)

korg_sysex = do
    Parse.start_sysex Parse.korg_code
    Parse.match_bytes [0x30, 0x46]
    info <- current_program_dump
    Parse.end_sysex
    return info

current_program_dump = do
    Parse.match_bytes [0x40, 0x01]
    contents <- fmap dekorgify Parse.to_eox
    return $ korg_patch contents

korg_patch bytes = make_patch (name, category, (pb_up, pb_down), osc1, osc2)
    where
    -- These come from the sysex spec PROG_PRM.TXT from korg.
    common_off = 25 + 19*4 + 11*4
    osc1_off = common_off + 9
    osc2_off = osc1_off + 52
    [cat, u_pb_up, u_pb_down, osc1_type, osc2_type] =
        map (fromIntegral . (bytes!!))
        [16, common_off, common_off+1, osc1_off, osc2_off]
    pb_up = to_signed u_pb_up
    pb_down = to_signed u_pb_down
    name = Seq.strip $ Parse.to_string (take 16 bytes)
    category = Seq.at categories cat
    osc1 = Seq.at osc_types osc1_type
    osc2 = Seq.at osc_types osc2_type

-- TODO convert to 2s complement signed
to_signed = fromIntegral

make_patch (name, cat, pb_range, osc1, osc2) =
    -- Initialization will be filled in later.
    (Instrument.patch inst) { Instrument.patch_tags = tags }
    where
    inst = Instrument.instrument (Instrument.synth_name z1)
        name Nothing Control.empty_map pb_range
    tags = maybe_tags
        [("z1_category", cat), ("z1_osc", osc1), ("z1_osc", osc2)]

maybe_tags tags = [Instrument.tag k v | (k, Just v) <- tags]

categories =
    [ "Synth-Hard", "Synth-Soft", "Synth-Lead", "Synth-Motion", "Synth-Bass"
    , "E.Piano", "Organ", "Keyboard", "Bell", "Strings", "Bad/Choir"
    , "Brass", "Reed/Wind", "Guitar/Plucked", "Bass", "Percussive"
    , "Argpeggio", "SFX/Other"
    ]

osc_types =
    [ "standard", "comb", "vpm", "resonance", "ring mod", "cross mod", "sync"
    , "organ", "electric piano", "brass", "reed", "plucked", "bowed"
    ]

-- | Z1 sysexes use a scheme where the eighth bits are packed into a single
-- byte preceeding its 7 7bit bytes.
dekorgify :: [Word.Word8] -> [Word.Word8]
dekorgify [] = []
dekorgify (b7:bytes) =
    [copy_bit b7 i b | (i, b) <- zip [0..] b7group] ++ dekorgify rest
    where
    (b7group, rest) = List.splitAt 7 bytes
    copy_bit from i to = if Bits.testBit from i
        then Bits.setBit to 7 else Bits.clearBit to 7
