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

    "Solkattu.S" - Generic rhythmic framework, where the
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

    "Solkattu.Dsl.Solkattu" - Functions for creating solkattu scores.  It
    defines (or replaces) various operators to make scores look nicer.

    Solkattu.Score.Solkattu* - Instrument-independent korvais.

    "Solkattu.Dsl.Mridangam", Solkattu.Score.Mridangam* - These are similar to
    Dsl and Score.Solkattu*, except they use concrete mridangam strokes instead
    of abstract sollus.

    The naming convention is that \"Note\" is the level-specific value,
    which may have a \"Note\" constructor with the "next level" of value.
    \"SNote\" is an alias for composing Note with 'Sequence.Note', and
    \"Sequence\" is a newtype for a list of those, but is abstractly the monoid
    where you can put together notation to form a score.
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Solkattu.Solkattu where
import qualified Control.Exception as Exception
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack)

import qualified Util.CallStack as CallStack
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Styled as Styled

import qualified Derive.Expr as Expr
import qualified Solkattu.S as S
import qualified Solkattu.Tala as Tala

import           Global


{- | Render a concrete stroke to text representing it.  This is used for ASCII
    output, so it should produce only a single character per matra duration.
    There could be exceptions for strokes which are both rare and almost always
    occur before a rest.

    The Show and Pretty superclasses are to make debugging more convenient.
-}
class (Show a, Pretty a) => Notation a where
    notation :: a -> (Styled.Style, Text)
    -- | Extend the note to fill its time with this character.
    extension :: a -> Char
    extension _ = ' '

textNotation :: Text -> (Styled.Style, Text)
textNotation = (mempty,)

notationText :: Notation a => a -> Text
notationText = snd . notation

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

-- | A Group is metadata stored alongside the nested sollus, but the actual
-- nesting happens in 'S.Group'.  See NOTE [nested-groups] for how I arrived at
-- the design.
data Group = GReduction !Reduction | GMeta !Meta
    deriving (Eq, Ord, Show)

data Reduction = Reduction {
    -- | Where to split the sollus.
    _split :: !S.FMatra
    , _side :: !Side
    } deriving (Eq, Ord, Show)

data Meta = Meta {
    -- | This is the logical number of Matras the group has.  It has to be
    -- stored because the number of matras is only accurate relative to the
    -- tempo context.  For GSarva, this actually defines the duration, which
    -- is pretty unfortunate.  See 'flatDuration'.
    _matras :: !(Maybe S.Matra)
    -- | Normally name is derived from _matras and _type, but some groups want
    -- to override that.
    , _name :: !(Maybe Text)
    -- | This determines abstraction level and color highlight in the score.
    , _type :: !GroupType
    } deriving (Eq, Ord, Show)

meta :: GroupType -> Meta
meta = Meta Nothing Nothing

data GroupType =
    -- | A generic group, usually manually applied.
    GGroup
    -- | 'Reduction's get this automatically.
    | GReductionT
    -- | A bit of decorative filler, should be highlighted subtly if at all.
    | GFiller
    -- | A realized 'Pattern'.
    | GPattern
    -- | A pattern with sollus already given.
    | GExplicitPattern
    | GSarva
    -- | Check that this group has the duration in '_matras'.  This group type
    -- should be stripped out after the check.
    | GCheckDuration !S.Duration
    deriving (Eq, Ord, Show)

-- | All GroupTypes that should be seen by render.  GCheckDuration should
-- have been removed by 'Realize.checkDuration'.  I could express that in the
-- type, but it seems too noisy for now.
groupTypes :: [GroupType]
groupTypes = [GGroup, GReductionT, GFiller, GPattern, GExplicitPattern, GSarva]

instance Pretty GroupType where pretty = showt

-- | Before means drop the strokes before the '_split' split, After means
-- drop the ones after.
data Side = Before | After deriving (Eq, Ord, Show)
instance Pretty Side where pretty = showt

instance Pretty Group where
    pretty (GReduction r) = pretty r
    pretty (GMeta m) = pretty m
instance Pretty Reduction where
    pretty (Reduction split side) = pretty (split, side)
instance Pretty Meta where
    -- Shorthand that makes tests look nicer.
    pretty (Meta (Just matras) Nothing GSarva) = "==" <> showt matras
    pretty (Meta Nothing Nothing gtype) = pretty gtype
    pretty (Meta matras name gtype) = pretty (matras, name, gtype)

-- | A note that can take up a variable amount of space.  Since it doesn't have
-- set strokes (or any, in the case of Rest), it can be arbitrarily divided.
data Space = Rest
    -- | This is not actual rest time in the performance, but inserted in the
    -- score for a start offset.
    | Offset
    deriving (Eq, Ord, Show)

instance Pretty Space where
    pretty Rest = "__"
    pretty Offset = ".."

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
    -- | For tabla, mark a dha as always being on sur.
    | Sur
    deriving (Eq, Ord, Show)

instance Pretty Tag where
    pretty = \case
        Tag i -> pretty i
        Middle -> "mid"
        Standard -> "standard"
        Sur -> "sur"

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

newtype Pattern = PatternM S.Matra
    deriving (Eq, Ord, Show)

pattern :: S.Matra -> Pattern
pattern = PatternM

instance S.HasMatras Pattern where
    matrasOf (PatternM m) = m
    hasSustain _ = True

instance Pretty Pattern where pretty = notationText

instance Notation Pattern where
    notation (PatternM matras) = textNotation $ showt matras <> "p"
    extension _ = '-'

instance Expr.ToExpr Pattern where
    to_expr p = case p of
        PatternM matras -> Expr.generator $
            Expr.call "p" [Expr.to_val matras]

data Karvai = Karvai | NotKarvai deriving (Eq, Ord, Show)

data Sollu =
    NoSollu -- ^ a dummy sollu for rests in Konnakol
    | Cham | Dheem | Dhom | Di | Dim | Din | Dit | Du
    | Ga | Gin | Gu | Jo | Ka | Ki | Ku | Kum | Lang
    | Mi | Na | Nam | Nang | Nu | Ri
    | Ta | Tam | Tang | Tong | Tat | Tha | Thom | Ti
    deriving (Eq, Ord, Enum, Bounded, Show)

instance Notation Sollu where notation = textNotation . Text.toLower . showt
instance Pretty Sollu where pretty = notationText

-- ** parseSollus

parseSollus :: Text -> Either Error [Maybe Sollu]
parseSollus = parseSyllables True allSollus

allSollus :: [(Text, Sollu)]
allSollus = Lists.keyOn notationText $ filter (/= NoSollu) [minBound ..]

parseSyllables :: Show sollu => Bool -> [(Text, sollu)] -> Text
    -> Either Error [Maybe sollu]
parseSyllables elideN solluMap = fmap concat . mapM parse . Text.words
    where
    parse w = case parseSyllablesWord elideN solluMap w of
        [] -> Left $ "no parse for " <> showt w
        [sollus] -> Right sollus
        xs -> Left $ "multiple parses for " <> showt w <> ": " <> showt xs

parseSyllablesWord :: Bool -> [(Text, sollu)] -> Text -> [[Maybe sollu]]
parseSyllablesWord elideN solluMap = go
    where
    go prefix
        | Text.null prefix = [[]]
        | has "_" = map (Nothing :) (go (Text.drop 1 prefix))
        | otherwise = do
            (str, sollu) <- filter (has . fst) solluMap
            let suffix = Text.drop (Text.length str) prefix
            -- Allow an elided n, e.g. tadinginathom vs. tadinginnathom.
            suffix <- suffix : if elideN && "n" `Text.isSuffixOf` str
                then ["n" <> suffix] else []
            (Just sollu :) <$> go suffix
        where has = (`Text.isPrefixOf` prefix)

-- * durations

durationOf :: S.HasMatras a => S.Tempo -> S.Sequence Group a
    -> S.Duration
durationOf = _durationOf (\_ -> id)

matrasOf :: S.HasMatras a => S.Tempo -> S.Sequence Group a -> S.FMatra
matrasOf = _durationOf toMatras
    where toMatras tempo dur = realToFrac $ dur * fromIntegral (S._nadai tempo)

_durationOf :: (S.HasMatras a, Num dur, Ord dur)
    => (S.Tempo -> S.Duration -> dur) -> S.Tempo -> S.Sequence Group a -> dur
_durationOf convert tempo = go tempo . S.toList
    where
    go tempo = Num.sum . map (get tempo)
    get tempo = \case
        S.Note n -> convert tempo $ S.noteDuration tempo n
        S.TempoChange change notes -> go (S.changeTempo change tempo) notes
        S.Group (GReduction (Reduction splitAt side)) notes -> case side of
            Before -> max 0 (go tempo notes - split)
            After -> min split (go tempo notes)
            where split = convert tempo $ S.fmatraDuration tempo splitAt
        S.Group (GMeta (Meta (Just matras) _ _)) _notes ->
            convert tempo $ S.matraDuration tempo * fromIntegral matras
        S.Group (GMeta (Meta Nothing _ _)) notes -> go tempo notes

-- | Unfortunately, with Reduction and GSarva groups, the notes don't reflect
-- the actual durations, so for 'Group'-bearing 'S.Flat', I need a special
-- function.  I tried really hard to prevent this but failed.  The sollus have
-- to go in the note field, and with reductions and sarva, they no longer
-- correspond exactly to realized strokes.  I would have to two note slots,
-- one for a space-filling @Group FMatras@, and another for the sollus, but
-- since groups can be nested, it gets really head-hurting for my tiny brain.
--
-- See NOTE [nested-groups] for chaotic details.
flatDuration :: S.HasMatras a => S.Flat Group a -> S.Duration
flatDuration (S.FNote tempo note) = S.noteDuration tempo note
flatDuration (S.FGroup tempo group notes) = case group of
    GReduction (Reduction splitAt side) -> case side of
        Before -> max 0 (completeDur - split)
        After -> min split completeDur
        where
        split = S.fmatraDuration tempo splitAt
        completeDur = Num.sum (map flatDuration notes)
    GMeta (Meta (Just matras) _ _) ->
        S.matraDuration tempo * fromIntegral matras
    GMeta (Meta Nothing _ _) -> Num.sum (map flatDuration notes)

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
    -> S.Sequence g (Note sollu) -> [S.Sequence g (Note sollu)]
vary allowedVariations notes
    | null modificationGroups = [notes]
    | otherwise = map apply modificationGroups
    where
    -- List of sets of permutations.
    modificationGroups = permuteFst allowedVariations (findTriads notes)
    -- Apply a set of permutations to the original input.
    apply mods = S.fromList $ applyModifications
        (\_ matras -> S.Note (Pattern (PatternM matras)))
        (concatMap extract mods) (S.toList notes)
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
findTriads :: S.Sequence g (Note sollu) -> [(S.Matra, (Int, Int, Int))]
findTriads notes =
    [ (matras, triad)
    | (matras, indices) <- Lists.groupFst
        [ (matras, i)
        | (i, S.Note (Pattern (PatternM matras))) <- zip [0..] (S.toList notes)
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

throw :: HasCallStack => Text -> a
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

check :: HasCallStack => Either Error a -> a
check = either throw id

checkMsg :: HasCallStack => Text -> Either Error a -> a
checkMsg msg = either (throw . ((msg <> ": ") <>)) id

{- NOTE [nested-groups]

    I arrived at the design for groups after a lot of hassle and alternate
    implementations.  Specifically, 'Group' used to have the dropped sollus
    while the remaining sollus were in 'S.Group'.  This had the nice result
    that just summing up durations worked, with no knowledge of 'Group' needed.
    However, I was forced to switch to the current implementation where all
    sollus are in the sequence, and 'Group' simply documents how many sollus
    should be dropped after realization, due to difficulties dealing with
    nested groups.  I'll include my notes below in case I'm ever tempted to
    revisit this, and don't understand why I wound up with such a weird design.

    * I have a problem with nested groups, e.g. dropM 1 . dropM 1.
      . I could try to simplify away nested groups.  That means making
        each one absolute, so the outer one loses its sollus, and they go
        on the inner ones.  Of course if there aren't inner ones, then
        I gave to give them the group, and it seems complicated.
      . I could try to simplify just simple nested groups, where all the
        children are groups.
      . I could try to not produce them in the first place, so splitD
        could do the simplification.  I think it's the same, just built
        into splitD.
      . Or I could try to get realize to understand nested groups.
        I guess it would have to accumulate sollus when it sees a group,
        and put them on any sub-groups.  Still I'd like to simplify for
        the same reason I simplify tempo changes, which is to make the
        intermediate data less hairy.
      . For now, just make reduce not produce them.  Try simple
        simplification later.
      . The reason to make 'realize' understand nested groups is that then
        it works for other ways to wind up with them.  For example?
      . dropM 1 (ta . dropM 1 (di . ki))
      . But do I really want this to look for tadiki, or should it be taki?
        I need a realistic example.
        . Reduce a phrase, where one element has a replaceStart on it?
          No, because replaceStart explicitly doesn't use a group.
        . Nested reduction? reduce (reduce takita) =>
          takita kita ta, kita kita ta, ta kita ta, kita ta, ta ta, ta
        . These should definitely use takita.
        . But on the other hand, what about simultaneous reduction:
          takita dhomdhomka thom
            kita     dhomka thom
              ta         ka thom
        . I could model this as:
          zipWith (.thom) (reduce takita) (reduce dhomdhomka)
        . Ok, then what about dropM 1 (takita . dropM 1 dhomdhomka)
          The least confusing would be to look for takita and dhomdhomka,
          but with nested groups this would look for takita and tadhomdhomka,
          which is definitely no good.
        . But dropM 1 (dropM 1 takita) should look for takita, not kita.
        . Of course I'd actually write dropM 1 takita . dropM 1 dhomdhomka
          but maybe there are cases where I get the nested one accidentally?
        . What exactly is the sandi error about?
          t4 t3 t2 are each groups.  sandi (t3.t2) drops the first t3.t2 from
          the tri, which should reduce it to [].
        . How then does this become looking for t3 and t2 separately instead
          of t4?  I should wind up with:
            dropM 1 t4 . dropM 2 t4
            . dropM (5) (dropM 1 t4 . dropM 2 t4 . tat.__.tam.__ ...)
        . So the group winds up being (t3.t2.tat.__.tam.__)
        . Since in this case the t3.t2 is totally dropped, I could fix this
          by saying if I can find the suffix (tat.tam), then don't bother
          looking for the prefix (t3.t2).  But this in turn would cause groups
          with identical tails to match wrong, e.g. dropM 1 takita would match
          kita.  The key thing is I don't know if a match with the prefix
          would have included the suffix or not.
        . But not necessarily, because I first try with the prefix, and only
          try suffix only if there is no match with the prefix.
        . But even if this does work, it seems like a hack that will break as
          soon as I'm not happening to drop an entire group.  E.g. I could
          sandi away a fragment:
            reduce3 (taka.takadinna.naka)
            . sandi naka (tri_ din (naka.dinna))
              =>
            taka.takadinna.naka
                 takadinna.naka
                     dinna{naka
                           naka}dinna.din
                           naka.dinna.din
                           naka.dinna
        . This works though, it's just dropM 2 (naka.dinna).
        . But, if nested groups were implemented, then the dropM 5 add
          dropM 1 t4 . dropM 2 t4 back on, which in turn would turn into
          t4.t4, which matches on t4 as expected, even though they all wind up
          getting dropped.

      . It seems there are two ways to implement groups: distributed, and
        plain.  Distributed means given
          group x (a . group y b . group z c),
        'x' is distributed among the subgroups:
          a . group xy b . group xz c.
        Plain means it's just goes on the beginning:
          group x (a . group y b . group z c)
          group x a . group y b . group z c
          . If there's no 'a', i.e. directly nested groups, then the 'x' is
            lost.

      . Still, what about reduce (takita . dropM 1 theme . din)?
        It seems like it should match takita and theme.
        plain: match takita and theme        ==> good.
        distributed: match takita . ta.theme ==> bogus.
      . But it also seems like nested drops should work:
          dropM 1 (dropM 1 nakita).
        plain: group [na]+ kita ==> group [ki]+ (group [na] ta) ==> bogus.
        distributed: group [ki]+ (group [na] ta) ==> group [na, ki]+ ta ==> good

      . Unless I could specifically looked for directly nested groups, so:
          group [na] kita ==> group [na, ki]+ ta
        which is now unnested and sensible.  But it only works when both
        sides are the same, e.g.:
          group [na]+ kita ==> group +[ta] (group [na]+ ta)
        is back to needing distributed groups.

      . It's like I want distributed for directly nested groups, but not if
        there's something in between.  But is that too ad-hoc and complicated?
      . How could I even implement it?
        That would be the hack in Notation.splitD.

      . Ok, since I don't know what to do, and I think plain groups are
        currently implemented, let's just do that.  But:
        * Change Sequence._marks from Maybe (GroupMark g) to [GroupMark g] so
          I can represent directly nested groups at least.
        . realize looks for suffix if prefix++suffix doesn't match.
          . This should fix the sandi examples.
        . splitD implements the directly nested hack.
      . Actually it turns out the sandi situation is not what I thought:
        . Given (dropM 3 $ dropM 1 nakita <> nakita)
        . The dropM 3 drops the first group with the leading Na, so the
          fact that there was one is lost:
           group [na] [ki, ta] . [na, ki, ta] ==> group [ki, ta, na] [ki, ta]
        . Instead it could collect the [na] from the group at the front:
               ==> group [na, ki, ta, na] [ki, ta]
        . Alternately, I could omit the [ki, ta] from the dropped group from
          the prefix.  I guess the rule would be don't include parts of other
          groups in a group's prefix.
        . Is there any reason prefer one over the other?  The first would be
          better if I needed that prefix to match, and doesn't happen to drop
          a whole group, e.g. dropM 1 . dropM 1.
        . For both I need splitD_ to return something extra, either the extra
          prefix/suffix, or the number of sollus to leave off the
          prefix/suffix.

        . Another way is that the group prefix can have nested groups in it.
          Then they have to be expanded recursively.  Then
           dropM 3 $ group na [ki, ta] . [na, ki, ta]
              ==> group (group [na] [ki, ta] . na) [ki, ta]
        . Expanding recursively might not be as scary as it sounds, because
          it's just a full SequenceT, so I just tack it on in
          'Realize.realize_group', instead of faking up Solkattu.Notes.
        . Then I have Group sollu = [sollu] ==> [S.Note (Group sollu) sollu].
          It's still not exactly a SequenceT, so I still have to do some
          faking up to get it to one, specifically:
            S.Note (Solkattu.Group sollu) sollu -->
            S.Note (Solkattu.Group sollu) (Solkattu.Note sollu)
          That's not bad at all, fmap (Solkattu.Note . Solkattu.note) should
          do it.
        . So it seems possible, but worth it?  I think maybe so, because
          I don't need any ad-hoc rules like collect sollus from the first
          group... which breaks if there is a second group.
        . In fact, maybe this implements the non-distributed "plain" group
          technique already.  Actually no, it just makes it work from inside
          the _dropped.  I think it already works outside.
      / Change Solkattu._dropped to [S.Note (Group sollu) sollu].
        . It turns out S.Note (Solkattu.Group sollu) sollu ->
            S.Note (Solkattu.Group sollu) (Solkattu.Note sollu)
          above is wrong.  I need to go
            S.Note (Solkattu.Group sollu) sollu -> Solkattu.Note sollu
          It's because it's already flattened, so I need to flatten the
          _dropped.  But the groups have also been flattened out, so I can't
          do it unless the caller can not do that.
        . Realize.realize gets them as [(Meta, Solkattu.Note sollu)], so
          I need to go to (Meta, Solkattu.Note)
        . Oh and it turns out 'reassociate_strokes' uses Meta == Nothing to
          identify the strokes from _dropped, and stuff them back in again.
          I'll need another way to do that.
        . Presumably the stroke groups should now also retain group structure,
          even though I don't think I actually care what's in there.  Can
          I reassociate right after the realize and use the count?  I know how
          long 'extras' is
        . reassociate_strokes might even be wrong, because it always puts
          Nothings into the following Meta, but that's assuming everything is
          a Front group.
        . What if the dropM splits a group?  I'd have to put it back together,
          but if I make another S.Group the size has been lost.  But if I put
          it in flattened, then I can just mash them back together.  Well,
          except I'm still splitting the S.Group, so I lose the size anyway.
        . Maybe I can get around it by not splitting the group at all, just
          put in the complete sequence, along with a split point.  Then the
          job of realize is to drop the extra strokes after realizing them,
          and emit the group boundaries.
        . Actually I could leave that to a later step, and in fact leaving it
          generic might be best because if I do this then pretty printing can
          get confusing because the extra dropped bits are still in there, and
          having a way to strip them out could be useful.
      * Change Solkattu._dropped to (Side, Duration).
      * The realized output needs dropped strokes, since Technique relies on
        it.
      * I have to make matrasOf understand Groups.
      * realize c_17_08_29 gets an alignment error, but it looks right?
        . Because verify_alignment is using pre-degrouped notes.
      * _dropped Durations are wrong.  This is because they are taken as
        absolute, but of course they are relative.  Maybe I should keep
        Duration as absolute, and use FMatra as an explicitly relative
        matra-level fractional duration.
        . The wrinkle is nadai as always.  How can I know what a matra should
          be after a nadai change if I don't know the surrounding nadai?
        . It doesn't help that realize knows absolute timing, because I put
          the value in in the score, where I don't know.  So the only way is
          that the duration unit is always relative to the current tempo.
          If I say it's N nadai at s0, then I think it works, but then I'm
          back to FMatra being nadai-independent.  I think?
        . Realize.split_at can know that the _dropped is relative to the
          tempo in scope.  So if it says 2, then we take it as matras and
          convert to Duration.
        . But currently it's matras/4, so should it still work out the same,
          just /4?
        . Wait, maybe the problem is entirely different.  Both flatten the same:
          . su $ dropM 1 taka:
            [(1/4, Front)(s+1(ta ka))]
            [(([2 (1/4, Front)], s1n4), ta), (([], s1n4), ka)]
          . dropM 1 $ su taka:
            [s+1((1/4, Front)(ta ka))]
            [(([2 (1/4, Front)], s1n4), ta), (([], s1n4), ka)]
        . This is because both TempoChange and Group are merged into
          Meta, and the order is lost.  I need to change the representation
          to fix this.
        . data Meta g a = TempoChange tempo | Group (GroupMark g) | Notes [a]
        . This would totally change all that MetaNote stuff, and maps would
          have to be mapAccumLs with state.  But since I can now see that
          the TempoChange happens before the Group, I know the right tempo
          when stripping out group prefix/suffix.  When I see Group, I scale
          _dropped by the current tempo.
        . One problem is that now the group counts don't work, because there
          are non-Notes in there.  I suppose I could make the count be for
          non-meta events.
        . Why wasn't this a problem before?  Because splitD used to work
          over the nested S.Note structure, so the order was still present.
        . Another way to fix this but preserve the pairs format is to have
          ([Meta], a), but it could have [] for no tempo, and I still lose
          order of notes with meta... so it doesn't really help.

        . This is still annoying because previous code could just pass through
          meta without caring about tempo or groups, but now it has to
          explicitly deal with groups.
        . What about making Group still be nested?  The reason to flatten
          notes is so they all have tempo, and I can easily find sequences.
          But that doesn't hold for groups, because I don't match sequences
          across group boundaries anyway.
        . realize_patterns changes the number of notes, which makes the group
          count inaccurate.  I have to update it.
        . Using a non-flat group means I wouldn't have worry about getting
          count wrong.
        . I feel like collect_group does adding dropped time wrong.  It seems
          like I should be able to do all the groups recursively.
        . matrasOf is 12, should be 8*3 - 2 - 4 = 18/2 = 9
        . It's because I don't take tempo into account when calculating
          dropped matras.

        * Implement Realize.format_table.
        * Clean out the Pretty etc. gunk and commented out code.
        * compile build/debug/seq, fix exports

      - Another side-effect is that "can't split" errors are only detected
        on realization.  But if it's important to get them earlier, I could
        have splitD verify even if it doesn't use the result immediately.
-}
