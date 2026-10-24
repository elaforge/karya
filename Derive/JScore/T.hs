-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.JScore.T (
    module Derive.JScore.T
    , module Derive.TScore.T
) where
import qualified Data.Text as Text

import           Derive.TScore.T
    (Error(..), Pos(..), Time(..), err, fakePos, findPos, showError, showErrors)

import           Global


-- * pitch

-- | Pitch with octave hint, before inferring the octave.  Positive means
-- pitches go up, negative is down.
data ParsedPitch = ParsedPitch Int PitchClass
    deriving (Eq, Ord, Show)

data Pitch = Pitch { pitchOctave :: Octave, pitchPc :: PitchClass }
    deriving (Eq, Ord, Show)

-- | This is the instrument relative octave.  Add the instrument's base octave
-- for absolute octave.
type Octave = Int

data PitchClass = P1 | P2 | P3 | P4 | P5 | P6 | P7
    deriving (Eq, Ord, Show, Bounded, Enum)

pcChar :: PitchClass -> Char
pcChar = \case
    P1 -> '1'; P2 -> '2'; P3 -> '3'; P4 -> '4'; P5 -> '5'; P6 -> '6'; P7 -> '7'

charPc :: Char -> Maybe PitchClass
charPc = \case
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

addOct :: Octave -> Pitch -> Pitch
addOct oct (Pitch o pc) = Pitch (oct+o) pc

addPc :: Laras -> Int -> Pitch -> Pitch
addPc laras steps = (!! abs steps) . iterate step
    where
    up = steps >= 0
    step pitch = addPcAbs (stepAt pitch) pitch
    stepAt pitch =
        head $ drop (fromEnum (pitchPc pitch)) (cycle transpositions)
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

addPcAbs :: Int -> Pitch -> Pitch
addPcAbs steps (Pitch octave pc) = Pitch (octave + oct) pc2
    where (oct, pc2) = toEnumBounded $ fromEnum pc + steps

pitchDiff :: Pitch -> Pitch -> Int
pitchDiff (Pitch oct1 pc1) (Pitch oct2 pc2) =
    perOct * (oct1 - oct2) + (fromEnum pc1 - fromEnum pc2)
    where
    perOct = fromEnum (maxBound :: PitchClass) + 1

data Gatra pitch =
    Gatra (Balungan pitch) (Balungan pitch) (Balungan pitch) (Balungan pitch)
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty pitch => Pretty (Gatra pitch) where
    pretty = Text.intercalate "," . map pretty . gatraNotes
instance Pretty pitch => Pretty (Balungan pitch) where
    pretty (Balungan mp _) = maybe "." pretty mp

gatraNotes :: Gatra pitch -> [Balungan pitch]
gatraNotes (Gatra n1 n2 n3 n4) = [n1, n2, n3, n4]

data Balungan pitch = Balungan (Maybe pitch) (Maybe BalunganAnnotation)
    deriving (Eq, Show, Functor, Foldable, Traversable)

data BalunganAnnotation = Gong | Kenong
    deriving (Eq, Show)

seleh :: Gatra Pitch -> Maybe PitchClass
seleh (Gatra n1 n2 n3 n4) = msum $ map pcOf [n4, n3, n2, n1]
    where
    pcOf (Balungan (Just (Pitch _ pc)) _) = Just pc
    pcOf _ = Nothing

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
data Laras
    = SlendroNem | SlendroSanga | SlendroManyura
    | PelogNem | PelogLima | PelogBarang
    deriving (Eq, Show, Bounded, Enum)

-- | Along with Instrument, affects expected number of notes per barline.
data Irama = Lancar | Tanggung | Dadi | Wiled | Rangkep
    deriving (Eq, Ord, Enum, Bounded, Show)
data Instrument = GenderBarung | GenderPanerus | Siter
    deriving (Eq, Ord, Show, Bounded, Enum)

instance Pretty Irama where pretty = showt
instance Pretty Instrument where pretty = showt

data Bentuk = Gendhing | Ketawang | Ladrang | Lancaran
    deriving (Show, Eq)

type ParsedBlock = Block ParsedPitch (Maybe Tracks)

data Block pitch tracks = Block
    { blockGatra :: Gatra pitch
    -- | Usually zero or one name, possibly with a modifier such as gede or
    -- cilik.
    , blockNames :: [Text]
    -- | Like block_names, but inferred automatically.
    , blockInferred :: [Text]
    , blockTracks :: tracks
    , blockPos :: Pos
    } deriving (Eq, Show, Functor)

instance Pretty tracks => Pretty (Block Pitch tracks) where
    pretty b = "Block " <> Text.intercalate "-" (blockNames b)
        <> " " <> pretty (blockGatra b)
        <> " " <> pretty (blockTracks b)

newtype Tracks = Tracks [Track ParsedToken]
    deriving (Eq, Show)

data Track token = Track
    { trackTokens :: [token]
    , trackPos :: Pos
    } deriving (Eq, Show)

data Token pos note rest = TBarline pos | TNote pos note | TRest pos rest
    deriving (Eq, Ord, Show)

instance (Pretty note, Pretty rest) => Pretty (Token pos note rest) where
    pretty = \case
        TBarline {} -> "|"
        TNote _ note -> pretty note
        TRest _ rest -> pretty rest

tokenNote :: Token pos note rest -> Maybe note
tokenNote = \case
    TNote _ note -> Just note
    _ -> Nothing

tokenPos :: Token pos note rest -> pos
tokenPos = \case
    TBarline pos -> pos
    TNote pos _ -> pos
    TRest pos _ -> pos

setTokenPos :: a -> Token pos note rest -> Token a note rest
setTokenPos a = \case
    TBarline _ -> TBarline a
    TNote _ n -> TNote a n
    TRest _ r -> TRest a r

type ParsedToken = Token Pos (Note ParsedPitch HasSpace) Rest

mapNote :: (n1 -> n2) -> Token pos n1 rest -> Token pos n2 rest
mapNote f = \case
    TBarline pos -> TBarline pos
    TNote pos note -> TNote pos (f note)
    TRest pos rest -> TRest pos rest

mapPitch :: (a -> b) -> [Token pos (Note a dur) rest]
    -> [Token pos (Note b dur) rest]
mapPitch f = map $ mapNote (\n -> n { notePitch = f (notePitch n) })

data Note pitch dur = Note
    { notePitch :: pitch
    -- | The generated event should have 0 duration.
    , noteZeroDuration :: Bool
    , noteDuration :: dur
    } deriving (Eq, Ord, Show)

instance (Pretty pitch, Pretty dur) => Pretty (Note pitch dur) where
    pretty (Note pitch zeroDur dur) = mconcat
        [ "Note ", pretty pitch, if zeroDur then "/" else ""
        , " " <> pretty dur
        ]

-- | Keep track if there was whitespace after notes and rests.
-- I can use this to infer durations.
data HasSpace = HasSpace | NoSpace deriving (Eq, Ord, Show)

instance Pretty HasSpace where
    pretty HasSpace = "_"
    pretty NoSpace = "X"

data Rest = Rest
    { restSustain :: Bool
    , restSpace :: HasSpace
    } deriving (Eq, Ord, Show)

instance Pretty Rest where
    pretty r = if restSustain r then "." else "_"

-- * util

toEnumBounded :: forall a. (Bounded a, Enum a) => Int -> (Int, a)
toEnumBounded n = (i, toEnum r)
    where (i, r) = n `divMod` (fromEnum (maxBound :: a) + 1)
