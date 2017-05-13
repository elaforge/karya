-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Notation for Carnatic solkattu.

    This is actually a separate library that's independent of Derive.  The only
    connection is that its final output can be stroke names for some instrument
    and thus easily inserted into a track.

    Salkattu is a general form of rhythmic notation.  Since the syllables
    (sollus) are independent of any particular instrument, they can express
    general rhythmic structures, which can then be realized in a form idiomatic
    to different instruments.

    The system is split up in a somewhat complicated way to separate rhythmic
    handling from sollus, and separate realizations sollus to various
    instruments.  The structure from low to high level is:

    "Derive.Solkattu.Tala" - General 'Tala.Tala' type.

    "Derive.Solkattu.Sequence" - Generic rhythmic framework, where the
    "payload" note type is abstract.  This can express rhythms in terms of
    'S.Speed' and 'S.Nadai', check them against a Tala, and realize down to
    'S.Duration' tagged notes.

    "Derive.Solkattu.Solkattu" - Fill in a Sequence's note with a Sollu type.
    This supports all of the notation in "Derive.Solkattu.Dsl".  As Sequence
    leaves the note type abstract, this leaves the instrument-dependent stroke
    type abstract.

    "Derive.Solkattu.Realize" - This has an instrument-specific Stroke, which
    is the result of resolving the sollus.  The stroke type is still abstract
    since it's polymorphic over the specific instrument.

    "Derive.Solkattu.Mridangam", "Derive.Solkattu.KendangTunggal", etc. - These
    describe specific instruments for Realize.

    "Derive.Solkattu.Korvai" - A Korvai unifies the instrument-specific
    Patterns and StrokeMaps together with Tala and a solkattu sequence.  So I
    can support multiple instruments from one solkattu score, it merges the
    stroke types into a single type, and projects out the specific strokes
    depending on which instrument is being realized.

    "Derive.Solkattu.Dsl", "Derive.Solkattu.Notation" - Functions for creating
    solkattu scores.  Dsl defines (or replaces) various operators to make
    scores look nicer.

    "Derive.Solkattu.Score" - Instrument-independent korvais.

    "Derive.Solkattu.MridangamDsl", "Derive.Solkattu.MridangamScore" - These
    are similar to Dsl and Score, except they use concrete mridangam strokes
    instead of abstract sollus.

    The naming convention is that \"Note\" is the level-specific
    value, itself may have a \"Note\" constructor with the "next level" of
    value.  \"SNote\" is an alias for composing Note with 'Sequence.Note', and
    \"Sequence\" is an alias for a list of those, but is abstractly the monoid
    where you can put together notation to form a score.
-}
{-# LANGUAGE DeriveFunctor #-}
module Derive.Solkattu.Solkattu where
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import qualified Derive.Expr as Expr
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Solkattu.Sequence as S
import qualified Derive.Solkattu.Tala as Tala

import Global


type Error = Text

data Note stroke =
    Note Sollu Karvai (Maybe stroke)
    | Rest
    | Pattern !Pattern
    | Alignment !Tala.Akshara
    deriving (Eq, Ord, Show, Functor)

instance Pretty stroke => Pretty (Note stroke) where
    pretty n = case n of
        Note NoSollu karvai (Just stroke) -> mconcat
            [ pretty stroke
            , pretty_karvai karvai
            ]
        Note sollu karvai stroke -> mconcat
            [ pretty sollu
            , pretty_karvai karvai
            , maybe "" (("!"<>) . pretty) stroke
            ]
        Rest -> "__"
        Pattern p -> pretty p
        Alignment n -> "@" <> showt n
        where
        pretty_karvai Karvai = "(k)"
        pretty_karvai NotKarvai = ""

map_stroke :: Functor f => (Maybe a -> Maybe b) -> f (Note a) -> f (Note b)
map_stroke f = fmap $ \n -> case n of
    Rest -> Rest
    Pattern a -> Pattern a
    Alignment a -> Alignment a
    Note sollu karvai stroke -> Note sollu karvai (f stroke)

note_matras :: Note stroke -> S.Matra
note_matras s = case s of
    -- Karvai notes are cancelled out, so they logically have 0 duration.
    Note _ Karvai _ -> 0
    Note {} -> 1
    Rest -> 1
    Pattern p -> pattern_matras p
    Alignment {} -> 0

pattern_matras :: Pattern -> S.Matra
pattern_matras p = case p of
    PatternM m -> m
    Nakatiku -> 8

data Pattern =
    PatternM !S.Matra
    -- | 4-matra faran nakatikutarikita
    | Nakatiku
    deriving (Eq, Ord, Show)

instance Pretty Pattern where
    pretty (PatternM matras) = "p" <> showt matras
    pretty Nakatiku = "4n"

instance Expr.ToExpr Pattern where
    to_expr (PatternM matras) =
        Expr.generator (Expr.call "p" [ShowVal.show_val matras])
    to_expr Nakatiku = "na"

-- | If it's a karvai stroke, and it's followed by a rest, it will replace the
-- rest.  Otherwise, it will be replaced by a note.
data Karvai = Karvai | NotKarvai deriving (Eq, Ord, Show)

data Sollu =
    NoSollu -- ^ a dummy sollu for a 'Sollu' with an explicit stroke
    | Dheem | Dhom | Di | Din | Dit
    | Ga | Gin | Ka | Ki | Ku | Lang
    | Mi | Na | Nam | Nang | Ri | Ta | Tam | Tang
    | Tat | Tha | Thom | Ti
    deriving (Eq, Ord, Show)

instance Pretty Sollu where
    pretty = Text.toLower . showt

-- * durations

duration_of :: [S.Note (Note stroke)] -> S.Duration
duration_of = sum . map (S.note_duration note_matras S.default_tempo)

-- * functions

-- | A Karvai Note followed by a Rest will replace the rest, if followed by
-- a Note or Pattern, the Karvai will be dropped.  Since a 'Karvai' note
-- logically has no duration, if it's the last note it will be dropped
-- entirely.
cancel_karvai :: [(a, Note stroke)] -> [(a, Note stroke)]
cancel_karvai = go
    where
    go ((a, Note sollu Karvai stroke) : rest) = case drop_next_rest rest of
        (True, rest) -> (a, Note sollu NotKarvai stroke) : go rest
        (False, rest) -> go rest
    go (n:ns) = n : go ns
    go [] = []

drop_next_rest :: [(a, Note stroke)] -> (Bool, [(a, Note stroke)])
drop_next_rest (n : ns) = case snd n of
    Rest -> (True, ns)
    Note {} -> (False, n:ns)
    Pattern {} -> (False, n:ns)
    Alignment {} -> second (n:) $ drop_next_rest ns
drop_next_rest [] = (False, [])

-- | Verify that the notes start and end at sam, and the given Alignments
-- fall where expected.
verify_alignment :: Pretty stroke =>
    Tala.Tala -> [(S.Tempo, Note stroke)]
    -> ([(S.Tempo, Note stroke)], Maybe Error)
    -- ^ If there's an error, still return the elemnts leading up to it.
verify_alignment tala =
    first (filter (not . is_alignment . snd)) . S.right_until_left
        . map verify . S.tempo_to_state note_matras tala
        . (++[(S.default_tempo, Alignment 0)])
    where
    verify (state, Alignment akshara)
        | S.state_akshara state /= akshara || S.state_matra state /= 0 =
            Left $ "expected akshara " <> showt akshara
                <> ", but at " <> S.show_position state
    verify (state, note) = Right (S.state_tempo state, note)
    is_alignment (Alignment {}) = True
    is_alignment _ = False

-- * vary

type Variations = [(S.Matra, S.Matra, S.Matra)]

-- | Variation means replacing a triad of patterns of the same duration with a
-- an increasing or decreasing sequence.  For instance, 666 can become 567,
-- 765, or 777 can become 678 or 579 or their inverses.
--
-- TODO Variation on a higher order is also possible, so for instance 777, 777,
-- 777 may become 666, 777, 888
--
-- TODO Also we have 5, 55, 555 -> 55, 55, 55 -> 555, 55, 5.  This actually
-- applies to more than just Patterns, e.g. 3 as tadin_.  I think this is
-- orthogonal and could get a different function.
vary :: (S.Matra -> Variations) -- ^ variations allowed for this duration
    -> [S.Note (Note stroke)] -> [[S.Note (Note stroke)]]
vary allowed_variations notes
    | null modification_groups = [notes]
    | otherwise = map apply modification_groups
    where
    -- List of sets of permutations.
    modification_groups = permute_fst allowed_variations (find_triads notes)
    -- Apply a set of permutations to the original input.
    apply mods = apply_modifications
        (\_ matras -> S.Note (Pattern (PatternM matras)))
        (concatMap extract mods) notes
    extract ((m1, m2, m3), (i1, i2, i3)) = [(i1, m1), (i2, m2), (i3, m3)]

variations :: [(S.Matra, S.Matra, S.Matra) -> Bool] -> (S.Matra -> Variations)
variations filters = filter (\v -> all ($v) filters) . all_variations

ascending, descending, standard :: (S.Matra, S.Matra, S.Matra) -> Bool
ascending (m1, m2, m3) = m1 < m2 && m2 < m3
descending (m1, m2, m3) = m1 > m2 && m2 > m3
standard (m1, m2, m3) =
    m1 == m2 && m2 == m3
    || List.sort [m1, m2, m3] `elem` [[5, 6, 7], [6, 7, 8], [5, 7, 9]]

all_variations :: S.Matra -> Variations
all_variations matras = concatMap vars [0 .. max 1 (matras - min_duration)]
    where
    vars d
        | d == 0 = [(matras, matras, matras)]
        | otherwise =
            [ (matras - d, matras, matras + d)
            , (matras + d, matras, matras - d)
            ]
    min_duration = 3

-- | Find triples of Patterns with the same length and return their indices.
-- The indices are in ascending order.
find_triads :: [S.Note (Note stroke)] -> [(S.Matra, (Int, Int, Int))]
find_triads notes =
    [ (matras, triad)
    | (matras, indices) <- Seq.group_fst
        [ (matras, i)
        | (i, S.Note (Pattern (PatternM matras))) <- zip [0..] notes
        ]
    , triad <- triads indices
    ]
    where
    triads (x1:x2:x3:xs) = (x1, x2, x3) : triads xs
    triads _ = []

-- * util

apply_modifications :: (a -> mod -> a) -> [(Int, mod)]
    -- ^ modifications along with their indices, in ascending order
    -> [a] -> [a]
apply_modifications apply mods = go mods . zip [0..]
    where
    go [] xs = map snd xs
    go _ [] = []
    go ((i1, mod) : mods) ((i2, x) : xs)
        | i1 < i2 = go mods ((i2, x) : xs)
        | i1 == i2 = apply x mod : go mods xs
        | otherwise = x : go ((i1, mod) : mods) xs

permute_fst :: (a -> [b]) -> [(a, x)] -> [[(b, x)]]
permute_fst _ [] = []
permute_fst permutations ((k, x) : xs)
    | null xs = [[(p, x)] | p <- permutations k]
    | otherwise =
        [(p, x) : rest | p <- permutations k, rest <- go xs]
    where go = permute_fst permutations

check :: CallStack.Stack => Either Error a -> a
check = either errorStack id

check_msg :: CallStack.Stack => Text -> Either Error a -> a
check_msg msg = either (errorStack . ((msg <> ": ") <>)) id
