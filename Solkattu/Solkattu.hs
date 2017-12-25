-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Notation for Carnatic solkattu.

    This is actually a separate library that's independent of the rest of the
    sequencer.  The only connection is that its final output can be stroke
    names for some instrument and thus easily inserted into a track.

    Solkattu is a general form of rhythmic notation.  Since the syllables
    (sollus) are independent of any particular instrument, they can express
    general rhythmic structures, which can then be realized in a form idiomatic
    to different instruments.

    The system is split up in a somewhat complicated way to separate rhythmic
    handling from sollus, and separate realizations sollus to various
    instruments.  The structure from low to high level is:

    "Solkattu.Tala" - General 'Tala.Tala' type.

    "Solkattu.Sequence" - Generic rhythmic framework, where the
    "payload" note type is abstract.  This can express rhythms in terms of
    'S.Speed' and 'S.Nadai', check them against a Tala, and realize down to
    'S.Duration' tagged notes.

    "Solkattu.Solkattu" - Fill in a Sequence's note with a Sollu type.
    This supports all of the notation in "Solkattu.Dsl".  As Sequence
    leaves the note type abstract, this leaves the instrument-dependent stroke
    type abstract.

    "Solkattu.Realize" - This has an instrument-specific Stroke, which
    is the result of resolving the sollus.  The stroke type is still abstract
    since it's polymorphic over the specific instrument.

    "Solkattu.Instrument.Mridangam",
    "Solkattu.Instrument.KendangTunggal", etc. - These describe specific
    instruments for Realize.

    "Solkattu.Korvai" - A Korvai unifies the instrument-specific
    Patterns and StrokeMaps together with Tala and a solkattu sequence.  So I
    can support multiple instruments from one solkattu score, it merges the
    stroke types into a single type, and projects out the specific strokes
    depending on which instrument is being realized.

    "Solkattu.Dsl", "Solkattu.SolkattuGlobal",
    "Solkattu.Notation" - Functions for creating solkattu scores.  Dsl
    defines (or replaces) various operators to make scores look nicer.

    Solkattu.Score.Solkattu* - Instrument-independent korvais.

    "Solkattu.MridangamGlobal", Solkattu.Score.Mridangam* - These are similar
    to Dsl and Score.Solkattu*, except they use concrete mridangam strokes
    instead of abstract sollus.

    The naming convention is that \"Note\" is the level-specific
    value, itself may have a \"Note\" constructor with the "next level" of
    value.  \"SNote\" is an alias for composing Note with 'Sequence.Note', and
    \"Sequence\" is an alias for a list of those, but is abstractly the monoid
    where you can put together notation to form a score.
-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
module Solkattu.Solkattu where
import qualified Control.Exception as Exception
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import qualified Derive.Expr as Expr
import qualified Derive.ShowVal as ShowVal
import qualified Solkattu.Sequence as S
import qualified Solkattu.Tala as Tala

import Global


{- | Render a concrete stroke to text representing it.  This is used for ASCII
    output, so it should produce only a single character per matra duration.
    There could be exceptions for strokes which are both rare and almost always
    occur before a rest.

    TODO I could extend it with an html method for non-ASCII output, or some
    type that can be rendered to both ASCII and terminal escape codes.
-}
class Notation a where
    notation :: a -> Text

type Error = Text

data Note sollu =
    Note (NoteT sollu)
    | Space !Space
    | Pattern !Pattern
    | Alignment !Tala.Akshara
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Pretty sollu => Pretty (Note sollu) where
    pretty n = case n of
        Note note -> pretty note
        Space space -> pretty space
        Pattern p -> pretty p
        Alignment n -> "@" <> showt n

data Group = Group {
    -- | Where to split the sollus.
    _split :: !S.FMatra
    , _side :: !Side
    } deriving (Eq, Ord, Show)

-- | Before means drop the strokes before the '_split' split, After means
-- drop the ones after.
data Side = Before | After deriving (Eq, Ord, Show)
instance Pretty Side where pretty = showt

instance Pretty Group where
    pretty (Group dropped side) = pretty (dropped, side)

-- | A note that can take up a variable amount of space.  Since it doesn't have
-- set strokes (or any, in the case of Rest), it can be arbitrarily divided.
data Space = Rest | Sarva
    deriving (Eq, Ord, Show)

instance Pretty Space where
    pretty Rest = "__"
    pretty Sarva = "=="

data NoteT sollu = NoteT {
    _sollu :: !sollu
    -- | If it's a karvai sollu, and it's followed by a rest, it will replace
    -- the rest.  Otherwise, it will be replaced by a note.
    , _karvai :: !Bool
    -- | Tag a sequence for alternate realization.
    , _tag :: !(Maybe Tag)
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | A sollu can have a tag attached.  This is used to map certain sets of
-- sollus to a different realization.  The idea is that even though the sollus
-- are the same, they may be realized different ways in different contexts.
data Tag = Tag !Int
    -- | Marks the middle karvai in a tirmanam.  This is applied automatically,
    -- so it can have an alternate realization.
    | Middle
    -- | Marks a standard pattern.  This isolates the "standard pattern" use
    -- of common sollus like taka.
    | Standard
    deriving (Eq, Ord, Show)

instance Pretty Tag where
    pretty (Tag i) = pretty i
    pretty Middle = "mid"
    pretty Standard = "standard"

instance Num Tag where
    fromInteger = Tag . fromInteger
    -- These are awkward, but I want fromInteger, but see no reason to allow
    -- math on tags.
    (+) = error "tags aren't numbers"
    (-) = error "tags aren't numbers"
    (*) = error "tags aren't numbers"
    negate = error "tags aren't numbers"
    abs = error "tags aren't numbers"
    signum = error "tags aren't numbers"

note :: sollu -> NoteT sollu
note sollu = NoteT { _sollu = sollu, _karvai = False, _tag = Nothing }

noteOf :: Note sollu -> Maybe (NoteT sollu)
noteOf (Note n) = Just n
noteOf _ = Nothing

solluOf :: Note sollu -> Maybe sollu
solluOf = fmap _sollu . noteOf

instance Pretty sollu => Pretty (NoteT sollu) where
    pretty (NoteT sollu karvai tag) = mconcat
        [ prettyTag tag
        , pretty sollu
        , prettyKarvai karvai
        ]
        where
        prettyKarvai k = if k then "(k)" else ""
        prettyTag = maybe "" ((<>"^") . pretty)

modifyNote :: (NoteT a -> NoteT b) -> Note a -> Note b
modifyNote f n = case n of
    Note note -> Note (f note)
    Space space -> Space space
    Pattern p -> Pattern p
    Alignment n -> Alignment n

instance S.HasMatras (Note sollu) where
    matrasOf n = case n of
        -- Karvai notes are cancelled out, so they logically have 0 duration.
        Note note -> if _karvai note then 0 else 1
        Space {} -> 1
        Pattern p -> S.matrasOf p
        Alignment {} -> 0
    hasSustain n = case n of
        Note {} -> False
        Space {} -> True
        Pattern {} -> True
        Alignment {} -> False

instance S.HasMatras Pattern where
    matrasOf p = case p of
        PatternM m -> m
        Nakatiku -> 8
    hasSustain _ = True

data Pattern =
    PatternM !S.Matra
    -- | 4-matra faran nakatikutarikita
    | Nakatiku
    deriving (Eq, Ord, Show)

instance Pretty Pattern where pretty = notation

instance Notation Pattern where
    notation p = case p of
        PatternM matras -> "p" <> showt matras
        Nakatiku -> "4n"

instance Expr.ToExpr Pattern where
    to_expr p = case p of
        PatternM matras ->
            Expr.generator (Expr.call "p" [ShowVal.show_val matras])
        Nakatiku -> "na"

data Karvai = Karvai | NotKarvai deriving (Eq, Ord, Show)

data Sollu =
    NoSollu -- ^ a dummy sollu for rests in Konnakol
    | Dheem | Dhom | Di | Din | Dit
    | Ga | Gin | Gu | Ka | Ki | Ku | Kum | Lang
    | Mi | Na | Nam | Nang | Ri
    | Ta | Tam | Tang | Tat | Tha | Thom | Ti
    deriving (Eq, Ord, Show)

instance Notation Sollu where notation = Text.toLower . showt
instance Pretty Sollu where pretty = notation

-- * durations

durationOf :: S.HasMatras a => S.Tempo -> [S.Note Group a] -> S.Duration
durationOf = _durationOf (\_ -> id)

matrasOf :: S.HasMatras a => S.Tempo -> [S.Note Group a] -> S.FMatra
matrasOf = _durationOf toMatras
    where toMatras tempo dur = realToFrac $ dur * fromIntegral (S._nadai tempo)

_durationOf :: (S.HasMatras a, Num dur, Ord dur)
    => (S.Tempo -> S.Duration -> dur) -> S.Tempo -> [S.Note Group a] -> dur
_durationOf convert = go
    where
    go tempo = sum . map (get tempo)
    get tempo n = case n of
        S.Note n -> convert tempo $ S.noteDuration tempo n
        S.TempoChange change notes -> go (S.changeTempo change tempo) notes
        S.Group g notes -> case _side g of
            Before -> max 0 (go tempo notes - split)
            After -> min split (go tempo notes)
            where split = convert tempo $ S.fmatraDuration tempo (_split g)

-- * functions

-- | A Karvai Note followed by a Space will replace the rest, if followed by
-- a Note or Pattern, the Karvai will be dropped.  Since a 'Karvai' note
-- logically has no duration, if it's the last note it will be dropped
cancelKarvai :: [S.Flat g (Note sollu)] -> [S.Flat g (Note sollu)]
cancelKarvai ns = fst $ State.runState (go ns []) False
    where
    -- This is way too complicated because Groups are nested.  The problem is
    -- that I want to look at and possibly modify a future note.  If Flat were
    -- really flat, then I could just look down the list and modify.  Future is
    -- so the last note of a group can still see future notes.  If I see a
    -- rest in the future, I emit the karvai note and turn on a "suppress next
    -- rest" bit.
    go (S.FGroup tempo g children : notes) future = do
        children <- go children (notes ++ future)
        (S.FGroup tempo g children :) <$> go notes future
    go (S.FNote tempo (Note note) : notes) future | _karvai note =
        if nextRest (S.flattenedNotes (notes ++ future))
            then suppress
                >> (S.FNote tempo (Note (note { _karvai = False })) :)
                    <$> go notes future
            else go notes future
    go (note@(S.FNote _ (Space Rest)) : notes) future = ifM isSuppressed
        (go notes future) ((note:) <$> go notes future)
    go (n : ns) future = (n:) <$> go ns future
    go [] _ = return []

    suppress = State.put True
    isSuppressed = State.get <* State.put False
    nextRest [] = False
    nextRest (n : ns) = case n of
        Space Rest -> True
        Alignment {} -> nextRest ns
        _ -> False

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
    -> [S.Note g (Note sollu)] -> [[S.Note g (Note sollu)]]
vary allowedVariations notes
    | null modificationGroups = [notes]
    | otherwise = map apply modificationGroups
    where
    -- List of sets of permutations.
    modificationGroups = permuteFst allowedVariations (findTriads notes)
    -- Apply a set of permutations to the original input.
    apply mods = applyModifications
        (\_ matras -> S.Note (Pattern (PatternM matras)))
        (concatMap extract mods) notes
    extract ((m1, m2, m3), (i1, i2, i3)) = [(i1, m1), (i2, m2), (i3, m3)]

variations :: [(S.Matra, S.Matra, S.Matra) -> Bool] -> (S.Matra -> Variations)
variations filters = filter (\v -> all ($v) filters) . allVariations

ascending, descending, standard :: (S.Matra, S.Matra, S.Matra) -> Bool
ascending (m1, m2, m3) = m1 < m2 && m2 < m3
descending (m1, m2, m3) = m1 > m2 && m2 > m3
standard (m1, m2, m3) =
    m1 == m2 && m2 == m3
    || List.sort [m1, m2, m3] `elem` [[5, 6, 7], [6, 7, 8], [5, 7, 9]]

allVariations :: S.Matra -> Variations
allVariations matras = concatMap vars [0 .. max 1 (matras - minDuration)]
    where
    vars d
        | d == 0 = [(matras, matras, matras)]
        | otherwise =
            [ (matras - d, matras, matras + d)
            , (matras + d, matras, matras - d)
            ]
    minDuration = 3

-- | Find triples of Patterns with the same length and return their indices.
-- The indices are in ascending order.
findTriads :: [S.Note g (Note sollu)] -> [(S.Matra, (Int, Int, Int))]
findTriads notes =
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

-- * exceptions

-- | Yes, I use impure exceptions, because otherwise the DSL has to become
-- monadic or at least applicative.  But it seems less egregious because there
-- isn't such a strong distinction between compiling and running anyway.
--
-- But it does mean I have to be careful to force and catch at the boundaries.
newtype Exception = Exception Text
    deriving (Eq)
instance Exception.Exception Exception
instance Show Exception where
    show (Exception msg) = Text.unpack msg

throw :: CallStack.Stack => Text -> a
throw = CallStack.throw Exception

-- * util

applyModifications :: (a -> mod -> a) -> [(Int, mod)]
    -- ^ modifications along with their indices, in ascending order
    -> [a] -> [a]
applyModifications apply mods = go mods . zip [0..]
    where
    go [] xs = map snd xs
    go _ [] = []
    go ((i1, mod) : mods) ((i2, x) : xs)
        | i1 < i2 = go mods ((i2, x) : xs)
        | i1 == i2 = apply x mod : go mods xs
        | otherwise = x : go ((i1, mod) : mods) xs

permuteFst :: (a -> [b]) -> [(a, x)] -> [[(b, x)]]
permuteFst _ [] = []
permuteFst permutations ((k, x) : xs)
    | null xs = [[(p, x)] | p <- permutations k]
    | otherwise =
        [(p, x) : rest | p <- permutations k, rest <- go xs]
    where go = permuteFst permutations

check :: CallStack.Stack => Either Error a -> a
check = either errorStack id

checkMsg :: CallStack.Stack => Text -> Either Error a -> a
checkMsg msg = either (errorStack . ((msg <> ": ") <>)) id
