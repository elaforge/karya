-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.TScore.Java.T (
    module Derive.TScore.Java.T
    , module Derive.TScore.T
) where
import           Derive.TScore.T
    (Directive(..), Error(..), show_error, Pos(..), Time(..), fake_pos)

import           Global


newtype Score = Score [(Pos, Toplevel)]
    deriving (Eq, Show)

data Toplevel = ToplevelDirective Directive | BlockDefinition Block
    deriving (Eq, Show)

data Pitch oct = Pitch oct PitchClass
    deriving (Eq, Show)
type Octave = Int
newtype RelativeOctave = RelativeOctave Int
    deriving (Eq, Show)

data PitchClass = P1 | P2 | P3 | P4 | P5 | P6 | P7
    deriving (Eq, Ord, Show, Bounded, Enum)

pc_char :: PitchClass -> Char
pc_char = \case
    P1 -> '1'; P2 -> '2'; P3 -> '3'; P4 -> '4'; P5 -> '5'; P6 -> '6'; P7 -> '7'

char_pc :: Char -> Maybe PitchClass
char_pc = \case
    '1' -> Just P1
    '2' -> Just P2
    '3' -> Just P3
    '4' -> Just P4
    '5' -> Just P5
    '6' -> Just P6
    '7' -> Just P7
    _ -> Nothing

data Gatra = Gatra Balungan Balungan Balungan Balungan
    deriving (Eq, Show)

data Balungan =
    Balungan (Maybe (Pitch RelativeOctave)) (Maybe BalunganAnnotation)
    deriving (Eq, Show)

data BalunganAnnotation = Gong | Kenong
    deriving (Eq, Show)

data Block = Block {
    block_gatra :: Gatra
    , block_names :: [Text]
    , block_tracks :: Maybe Tracks
    } deriving (Eq, Show)

newtype Tracks = Tracks [Track]
    deriving (Eq, Show)

data Track = Track {
    track_tokens :: [ParsedToken]
    , track_pos :: Pos
    } deriving (Eq, Show)

data Token note rest = TBarline Pos | TNote Pos note | TRest Pos rest
    deriving (Eq, Show)

token_pos :: Token note rest -> Pos
token_pos = \case
    TBarline pos -> pos
    TNote pos _ -> pos
    TRest pos _ -> pos

type ParsedToken = Token (Note (Pitch RelativeOctave) HasSpace) Rest

map_note :: (n1 -> n2) -> Token n1 rest -> Token n2 rest
map_note f = \case
    TBarline pos -> TBarline pos
    TNote pos note -> TNote pos (f note)
    TRest pos rest -> TRest pos rest

data Note pitch dur = Note {
    note_pitch :: pitch
    -- | The generated event should have 0 duration.
    , note_zero_duration :: Bool
    , note_duration :: dur
    -- | This is redundant with 'TNote's Pos, but convenient, since Check will
    -- later strip away 'Token's.
    , note_pos :: Pos
    } deriving (Eq, Show)

-- | Keep track if there was whitespace after notes and rests.
-- I can use this to infer durations.
data HasSpace = HasSpace | NoSpace deriving (Eq, Show)

data Rest = Rest {
    rest_sustain :: Bool
    , rest_space :: HasSpace
    } deriving (Eq, Show)
