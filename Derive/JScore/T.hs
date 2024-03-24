-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.JScore.T (
    module Derive.JScore.T
    , module Derive.TScore.T
) where
import           Derive.TScore.T
    (Error(..), Pos(..), Time(..), fake_pos, find_pos, show_error)

import           Global


-- * pitch

-- | Pitch with octave hint, before inferring the octave.  Positive means
-- pitches go up, negative is down.
data ParsedPitch = ParsedPitch Int PitchClass
    deriving (Eq, Ord, Show)

data Pitch = Pitch { pitch_octave :: Octave, pitch_pc :: PitchClass }
    deriving (Eq, Ord, Show)

-- | This is the instrument relative octave.  Add the instrument's base octave
-- for absolute octave.
type Octave = Int

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

-- TODO use the format_pitch one with dots?
instance Pretty Pitch where
    pretty (Pitch oct pc) = showt oct <> pretty pc
instance Pretty PitchClass where pretty = txt . drop 1 . show

add_oct :: Octave -> Pitch -> Pitch
add_oct oct (Pitch o pc) = Pitch (oct+o) pc

add_pc :: Laras -> Int -> Pitch -> Pitch
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

add_pc_abs :: Int -> Pitch -> Pitch
add_pc_abs steps (Pitch octave pc) = Pitch (octave + oct) pc2
    where (oct, pc2) = toEnumBounded $ fromEnum pc + steps

pitch_diff :: Pitch -> Pitch -> Int
pitch_diff (Pitch oct1 pc1) (Pitch oct2 pc2) =
    per_oct * (oct1 - oct2) + (fromEnum pc1 - fromEnum pc2)
    where
    per_oct = fromEnum (maxBound :: PitchClass) + 1

data Gatra pitch =
    Gatra (Balungan pitch) (Balungan pitch) (Balungan pitch) (Balungan pitch)
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Balungan pitch = Balungan (Maybe pitch) (Maybe BalunganAnnotation)
    deriving (Eq, Show, Functor, Foldable, Traversable)

data BalunganAnnotation = Gong | Kenong
    deriving (Eq, Show)

seleh :: Gatra Pitch -> Maybe PitchClass
seleh (Gatra n1 n2 n3 n4) = msum $ map pc_of [n4, n3, n2, n1]
    where
    pc_of (Balungan (Just (Pitch _ pc)) _) = Just pc
    pc_of _ = Nothing

-- * score

newtype Score block = Score [(Pos, Toplevel block)]
    deriving (Eq, Show, Functor, Foldable, Traversable)

type ParsedScore = Score ParsedBlock

data Toplevel block = ToplevelMeta Meta | BlockDefinition block
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Meta = Source Text | Piece Text | Section Text | Laras Laras
    | Irama Irama | Instrument Instrument
    deriving (Eq, Show)

-- | Actually laras + pathet.
data Laras =
    SlendroNem | SlendroSanga | SlendroManyura
    | PelogNem | PelogLima | PelogBarang
    deriving (Eq, Show, Bounded, Enum)

-- | Along with Instrument, affects expected number of notes per barline.
data Irama = Lancar | Tanggung | Dadi | Wiled | Rangkep
    deriving (Eq, Ord, Enum, Bounded, Show)
data Instrument = GenderBarung | GenderPanerus | Siter
    deriving (Eq, Show, Bounded, Enum)

type ParsedBlock = Block ParsedPitch (Maybe Tracks)

data Block pitch tracks = Block {
    block_gatra :: Gatra pitch
    -- | Usually zero or one name, possibly with a modifier such as gede or
    -- cilik.
    , block_names :: [Text]
    -- | Like block_names, but inferred automatically.
    , block_inferred :: [Text]
    , block_tracks :: tracks
    , block_pos :: Pos
    } deriving (Eq, Show, Functor)

newtype Tracks = Tracks [Track ParsedToken]
    deriving (Eq, Show)

data Track token = Track {
    track_tokens :: [token]
    , track_pos :: Pos
    } deriving (Eq, Show)

data Token pos note rest = TBarline pos | TNote pos note | TRest pos rest
    deriving (Eq, Ord, Show)

instance (Pretty note, Pretty rest) => Pretty (Token pos note rest) where
    pretty = \case
        TBarline {} -> "|"
        TNote _ note -> pretty note
        TRest _ rest -> pretty rest

token_note :: Token pos note rest -> Maybe note
token_note = \case
    TNote _ note -> Just note
    _ -> Nothing

token_pos :: Token pos note rest -> pos
token_pos = \case
    TBarline pos -> pos
    TNote pos _ -> pos
    TRest pos _ -> pos

set_token_pos :: a -> Token pos note rest -> Token a note rest
set_token_pos a = \case
    TBarline _ -> TBarline a
    TNote _ n -> TNote a n
    TRest _ r -> TRest a r

type ParsedToken = Token Pos (Note ParsedPitch HasSpace) Rest

map_note :: (n1 -> n2) -> Token pos n1 rest -> Token pos n2 rest
map_note f = \case
    TBarline pos -> TBarline pos
    TNote pos note -> TNote pos (f note)
    TRest pos rest -> TRest pos rest

map_pitch :: (a -> b) -> [Token pos (Note a dur) rest]
    -> [Token pos (Note b dur) rest]
map_pitch f = map $ map_note (\n -> n { note_pitch = f (note_pitch n) })

data Note pitch dur = Note {
    note_pitch :: pitch
    -- | The generated event should have 0 duration.
    , note_zero_duration :: Bool
    , note_duration :: dur
    } deriving (Eq, Ord, Show)

instance Pretty pitch => Pretty (Note pitch dur) where
    pretty = pretty . note_pitch

-- | Keep track if there was whitespace after notes and rests.
-- I can use this to infer durations.
data HasSpace = HasSpace | NoSpace deriving (Eq, Ord, Show)

data Rest = Rest {
    rest_sustain :: Bool
    , rest_space :: HasSpace
    } deriving (Eq, Ord, Show)

instance Pretty Rest where
    pretty r = if rest_sustain r then "." else "_"

-- * util

toEnumBounded :: forall a. (Bounded a, Enum a) => Int -> (Int, a)
toEnumBounded n = (i, toEnum r)
    where (i, r) = n `divMod` (fromEnum (maxBound :: a) + 1)
