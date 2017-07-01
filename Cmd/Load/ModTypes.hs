-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Types for modules.  Theoretically tracker-independent.
module Cmd.Load.ModTypes where
import qualified Data.Bits as Bits
import Data.Bits ((.&.))
import qualified Data.Either as Either
import Data.Word (Word8)

import qualified Derive.ScoreTypes as ScoreTypes
import qualified Perform.Pitch as Pitch
import Global


data Module = Module {
    _instruments :: [Instrument]
    , _default_tempo :: !Tempo
    , _blocks :: [Block]
    , _block_order :: [(Text, [Int])]
    } deriving (Eq, Show)

data Tempo = Tempo {
    _speed :: !Int -- ^ could be BPM, or some tempo value
    , _frames :: !Int -- ^ number of divisions per line
    } deriving (Eq, Show)

data Instrument = Instrument {
    _instrument_name :: !ScoreTypes.Instrument
    , _volume :: !(Maybe Int)
    } deriving (Eq, Show)

data Block = Block {
    -- | These are by-track.
    _tracks :: [[Line]]
    , _block_length :: !Int
    } deriving (Eq, Show)

data Line = Line {
    _pitch :: !(Maybe Pitch.NoteNumber)
    , _instrument :: !Int
    , _commands :: ![Command]
    } deriving (Eq, Show)

-- | 0 = no note
-- 1 = NN.c_1
pitch :: Int -> Maybe Pitch.NoteNumber
pitch p
    | p == 0 = Nothing
    | otherwise = Just $ fromIntegral (p - 1)

data Command =
    SlideUp !Int
    | SlideDown !Int
    | Portamento !Int
    | SetFrames !Int
    | Volume !Int
    | Crescendo !Int | Decrescendo !Int -- 0d xy
    | CutBlock
    | CutNote
    | DelayRepeat !Int !Int -- delay frames, repeat each n frames
    deriving (Eq, Show)

commands :: [(Int, Int)] -> ([Command], [(Int, Int)])
commands = Either.partitionEithers . mapMaybe make
    where
    make (0, _) = Nothing
    make (cmd, val) = Just $ maybe (Right (cmd, val)) Left
        (command (fromIntegral cmd) (fromIntegral val))

{- | Parse a Command.
    @
    01 xx -- slide up, 0 uses previous value
    02 xx -- slide down, 0 uses previous value
    03 xx -- portamento, 0 uses previous value
    04 xy -- vibrato, x speed, y depth
    05 xx -- slide + fade, like 0d but continue slide
    06 xx -- vibrato + fade
    07 xx -- tremolo
    08 xx -- hold + decay
    09 xx -- set tempo.frames
    0b xx -- jump
    0c xx - volume
    0d xy - x crescendo, y decrescendo
    0f 00 - cut block, after this line
    0f 01-f0 - set tempo
    0f f1 - play twice
    0f f2 - delay 1/2
    0f f3 - play thrice
    0f fa - pedal down
    0f fb - pedal up
    0f fe - end song
    0f ff - cut note
    11 - fine slide up
    12 - fine slide down
    14 - fine vibrato
    15 - set fine tune
    16 - loop
    18 - cut note at given frame
    19 - sample start offset *256 bytes
    1a - volue up
    1b - volume down
    1d - jump to next block + line
    1e - retrigger command
    1f xy - delay x frames, repeat every y

    00 - mod wheel
    0e - pan

    1f xx - delay
    @
-}
command :: Word8 -> Word8 -> Maybe Command
command cmd val = case cmd of
    0x03 -> Just $ Portamento $ int val
    0x0c -> Just $ Volume $ int val
    0x0d
        | val .&. 0x0f /= 0 -> Just $ Decrescendo $ int $ val .&. 0x0f
        | otherwise -> Just $ Crescendo $ int $ Bits.shiftR val 4 .&. 0x0f
    0x1f -> Just $ uncurry DelayRepeat $ split4 val
    0x0f
        | val == 0xff -> Just CutNote
        | val == 0x00 -> Just CutBlock
    _ -> Nothing
    where
    int = fromIntegral

split4 :: Word8 -> (Int, Int)
split4 w = (fromIntegral $ Bits.shiftR w 4 .&. 0xf, fromIntegral $ w .&. 0xf)
