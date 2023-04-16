-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Types for modules.  Theoretically tracker-independent.
module Cmd.Load.ModT where
import qualified Data.Bits as Bits
import           Data.Bits ((.&.))
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Word (Word8)

import qualified Util.Num as Num
import qualified Util.Lists as Lists
import qualified Derive.ScoreT as ScoreT
import qualified Perform.Pitch as Pitch

import           Global


data Module = Module {
    _instruments :: IntMap Instrument
    , _default_tempo :: !Tempo
    , _blocks :: [Block]
    -- | Name of sequence, '_blocks' indices.
    , _block_order :: Map Text [Int]
    } deriving (Eq, Show)

data Tempo = Tempo {
    _speed :: !Int -- ^ could be BPM, or some tempo value
    , _frames :: !Int -- ^ number of divisions per line
    } deriving (Eq, Show)

data Instrument = Instrument {
    _instrument_name :: !ScoreT.Instrument
    , _volume :: !(Maybe Double)
    } deriving (Eq, Show)

data Block = Block {
    _block_length :: !Int
    , _tracks :: [Track]
    } deriving (Eq, Show)

-- | Empty lines are common, so this uses IntMap as a sparse array.
type Track = IntMap Line

make_track :: [(Int, Line)] -> Track
make_track = carry_zeroes . IntMap.fromList

data Line = Line {
    _pitch :: !(Maybe Pitch.NoteNumber)
    , _instrument :: !Int
    , _commands :: ![Command]
    } deriving (Eq, Show)

-- | 0 = no note
-- 1 = NN.c_1.  This is right for MIDI instruments, but not for samples.
pitch :: Int -> Maybe Pitch.NoteNumber
pitch p
    | p == 0 = Nothing
    | otherwise = Just $ fromIntegral (p - 1)

data Command =
    Command !Text !Word8
    | SetFrames !Int
    | Volume !Double -- ^ 0 to 1
    | VolumeSlide !Double -- ^ positive for up, negative for down
    | CutBlock
    | CutNote
    | DelayRepeat !Int !Int -- ^ delay frames, repeat each n frames
    deriving (Eq, Show)

commands :: [(Int, Int)] -> [Command]
commands = mapMaybe make
    where
    make (0, _) = Nothing
    make (cmd, val) = Just $ med_command (fromIntegral cmd) (fromIntegral val)

-- | Parse a Command.
med_command :: Word8 -> Word8 -> Command
med_command cmd val = case cmd of
    0x0c -> Volume $ volume val
    0x0d
        | val .&. 0x0f /= 0 -> VolumeSlide $ negate $ int $ val .&. 0x0f
        | otherwise -> VolumeSlide $ int $ Bits.shiftR val 4 .&. 0x0f
    0x1a -> VolumeSlide $ int val
    0x1b -> VolumeSlide $ negate $ int val
    0x1f -> uncurry DelayRepeat $ split4 val
    0x0f -> case val of
        0x00 -> CutBlock
        0xf1 -> Command "play twice" 0
        0xf2 -> Command "delay 1/2" 0
        0xf3 -> Command "play thrice" 0
        0xfa -> Command "pedal down" 0
        0xfb -> Command "pedal up" 0
        0xfe -> Command "end song" 0
        0xff -> CutNote
        _ -> Command "set tempo" val
    _ | Just name <- Map.lookup cmd med_command_names -> Command name val
    _ | otherwise -> Command ("0x" <> Num.hex 2 cmd) val
    where
    int = fromIntegral

med_command_names :: Map Word8 Text
med_command_names = Map.fromList
    [ (0x01, "slide up")
    , (0x02, "slide down") -- 0 uses previous value
    , (0x03, "portamento") -- 0 uses previous value
    , (0x04, "vibrato") -- x speed, y depth
    , (0x05, "slide + fade") -- like 0d but continue slide")
    , (0x06, "vibrato + fade")
    , (0x07, "tremolo")
    , (0x08, "hold + decay")
    , (0x09, "set tempo.frames")
    , (0x0b, "jump")
    , (0x0c, "volume")
    , (0x0d, "x crescendo, y decrescendo")
    , (0x0f, "cut block, after this line")
    , (0x11, "fine slide up")
    , (0x12, "fine slide down")
    , (0x14, "fine vibrato")
    , (0x15, "set fine tune")
    , (0x16, "loop")
    , (0x18, "cut note at given frame")
    , (0x19, "sample start offset *256 bytes")
    , (0x1a, "volue up")
    , (0x1b, "volume down")
    , (0x1d, "jump to next block + line")
    , (0x1e, "retrigger command")
    , (0x1f, "xy - delay x frames, repeat every y")

    , (0x00, "mod wheel")
    , (0x0e, "pan")

    , (0x1f, "delay")
    ]

-- | MED volume goes from 0 to 0x64 (aka 100) instead of 0 to 0x40.
volume :: Integral a => a -> Double
volume = (/0x64) . fromIntegral

split4 :: Word8 -> (Int, Int)
split4 w = (fromIntegral $ Bits.shiftR w 4 .&. 0xf, fromIntegral $ w .&. 0xf)

-- | A Command arg of 0 is often a shorthand to carry forward the previous
-- value.  Eliminate this by doing the carry.
carry_zeroes :: Track -> Track
carry_zeroes =
    IntMap.fromAscList . snd . List.mapAccumL set (0, []) . IntMap.toAscList
    where
    set (inst, prev_cmds) (row, line) =
        ((_instrument line2, cmds), (row, line2 { _commands = cmds }))
        where
        line2 = set_inst inst line
        cmds = map (set_default prev_cmds) (_commands line)
    set_default prev_cmds cmd =
        fromMaybe cmd $ msum (map (flip carry_arg cmd) prev_cmds)
    -- If the prev_cmd was the same type, and this one has a 0, then
    -- replace with the prev_cmd.
    carry_arg (Command c1 arg) (Command c2 0) | c1 == c2 = Just $ Command c1 arg
    carry_arg (VolumeSlide arg) (VolumeSlide 0) = Just $ VolumeSlide arg
    carry_arg _ _ = Nothing
    set_inst inst line
        | _instrument line == 0 = line { _instrument = inst }
        | otherwise = line

-- * transform

map_instruments :: Map Text Text -> Module -> Module
map_instruments inst_map mod = mod { _instruments = set <$> _instruments mod }
    where
    set inst = inst
        { _instrument_name =
            ScoreT.Instrument $ Map.findWithDefault inst_name inst_name inst_map
        } where inst_name = ScoreT.instrument_name $ _instrument_name inst

transpose_instruments :: Map ScoreT.Instrument Pitch.NoteNumber
    -> Module -> Module
transpose_instruments transpose mod = flip modify_lines mod $ \line ->
    case (_pitch line, Map.lookup (_instrument line) by_num) of
        (Just nn, Just steps) -> line { _pitch = Just $ nn + steps }
        _ -> line
    where
    by_num = Map.fromList $ Lists.mapMaybeFst (flip Map.lookup inst_to_num) $
        Map.toList transpose
    inst_to_num = Map.fromList
        [ (_instrument_name inst, n)
        | (n, inst) <- IntMap.toList (_instruments mod)
        ]

modify_lines :: (Line -> Line) -> Module -> Module
modify_lines f = modify_tracks (fmap f)

modify_tracks :: (Track -> Track) -> Module -> Module
modify_tracks f mod = mod { _blocks = map tracks (_blocks mod) }
    where tracks block = block { _tracks = map f (_tracks block) }
