-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.TScore.Java.T (
    module Derive.TScore.Java.T
    , module Derive.TScore.T
) where
import           Derive.TScore.T
    (Directive(..), Error(..), Pos(..), Time(..), fake_pos, show_error)

import           Global


-- * pitch

data Pitch oct = Pitch { pitch_octave :: oct, pitch_pc :: PitchClass }
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

-- | Actually laras + pathet.
data Laras = Slendro | PelogNem | PelogLima | PelogBarang
    deriving (Eq, Show)

add_oct :: Octave -> Pitch Octave -> Pitch Octave
add_oct oct (Pitch o pc) = Pitch (oct+o) pc

add_pc :: Laras -> Int -> Pitch Octave -> Pitch Octave
add_pc laras steps = (!! abs steps) . iterate step
    where
    up = steps >= 0
    step pitch = add_pc_abs (step_at pitch) pitch
    step_at pitch =
        head $ drop (fromEnum (pitch_pc pitch)) (cycle transpositions)
    -- Yet another awkward diatonic transposition function.  This time I do it
    -- manual and hard coded, since there are so few pathet.  Why is this so
    -- hard?
    transpositions = case (laras, up) of
        --                       1  2  3  4  5  6  7
        (PelogBarang, True)  -> [1, 1, 2, 1, 1, 1, 2]
        (PelogBarang, False) -> [-1, -2, -1, -1, -2, -1, -1]
        --                       1  2  3  4  5  6  7
        (_, True)            -> [1, 1, 2, 1, 1, 2, 1]
        (_, False)           -> [-2, -1, -1, -1, -2, -1, -1]

add_pc_abs :: Int -> Pitch Octave -> Pitch Octave
add_pc_abs steps (Pitch octave pc) = Pitch (octave + oct) pc2
    where (oct, pc2) = toEnumBounded $ fromEnum pc + steps

pitch_diff :: Pitch Octave -> Pitch Octave -> Int
pitch_diff (Pitch oct1 pc1) (Pitch oct2 pc2) =
    per_oct * (oct1 - oct2) + (fromEnum pc1 - fromEnum pc2)
    where
    per_oct = fromEnum (maxBound :: PitchClass) + 1

data Gatra = Gatra Balungan Balungan Balungan Balungan
    deriving (Eq, Show)

data Balungan =
    Balungan (Maybe (Pitch RelativeOctave)) (Maybe BalunganAnnotation)
    deriving (Eq, Show)

data BalunganAnnotation = Gong | Kenong
    deriving (Eq, Show)

seleh :: Gatra -> Maybe PitchClass
seleh (Gatra n1 n2 n3 n4) = msum $ map pc_of [n4, n3, n2, n1]
    where
    pc_of (Balungan (Just (Pitch _ pc)) _) = Just pc
    pc_of _ = Nothing

-- * score

newtype Score block = Score [(Pos, Toplevel block)]
    deriving (Eq, Show)

type ParsedScore = Score ParsedBlock
type ParsedBlock = Block (Maybe Tracks)

data Toplevel block = ToplevelDirective Directive | BlockDefinition block
    deriving (Eq, Show)

data Block tracks = Block {
    block_gatra :: Gatra
    -- | Usually zero or one name, possibly with a modifier such as gede or
    -- cilik.
    , block_names :: [Text]
    -- | Like block_names, but inferred automatically.
    , block_inferred :: [Text]
    , block_tracks :: tracks
    } deriving (Eq, Show)

newtype Tracks = Tracks [Track ParsedToken]
    deriving (Eq, Show)

data Track token = Track {
    track_tokens :: [token]
    , track_pos :: Pos
    } deriving (Eq, Show)

data Token note rest = TBarline Pos | TNote Pos note | TRest Pos rest
    deriving (Eq, Show)

token_note :: Token note rest -> Maybe note
token_note = \case
    TNote _ note -> Just note
    _ -> Nothing

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

-- * util

toEnumBounded :: forall a. (Bounded a, Enum a) => Int -> (Int, a)
toEnumBounded n = (i, toEnum r)
    where (i, r) = n `divMod` (fromEnum (maxBound :: a) + 1)
