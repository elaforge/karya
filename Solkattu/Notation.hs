-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Generic combinators for solkattu patterns.  Because these are expected to
    be called as part of the DSL, impure exceptions are allowed, via
    'Solkattu.throw'.

    This is meant to have just Sequence manipulation, without
    instrument-specific functions.
-}
module Solkattu.Notation where
import Prelude hiding ((^), repeat)
import qualified Data.List as List

import qualified Util.CallStack as CallStack
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as S
import Solkattu.Sequence (Duration, FMatra)
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala

import Global


type SequenceT sollu = [S.Note Solkattu.Group (Solkattu.Note sollu)]
    -- This is the same as in Korvai.

-- * rests

class Rest a where __ :: a
instance Rest (SequenceT sollu) where
    __ = [S.Note __]
instance Rest (Realize.SNote sollu) where
    __ = Realize.rest
instance Rest (Solkattu.Note sollu) where
    __ = Solkattu.Space Solkattu.Rest

-- | These are meant to suffix a sollu.  Since the sollu is considered part of
-- the duration, the number is one higher than the number of rests.  E.g.
-- @din.__3@ is a 3 count, and equivalent to @din.__.__@.
__2, __3, __4, __5, __6, __7, __8, __9 :: SequenceT sollu
__2 = __
__3 = __n 3
__4 = __n 4
__5 = __n 5
__6 = __n 6
__7 = __n 7
__8 = __n 8
__9 = __n 9

__n :: S.Matra -> SequenceT sollu
__n n = repeat (n-1) __

restM :: S.Matra -> SequenceT sollu
restM n = repeat n __

sarvaM :: S.Matra -> SequenceT sollu
sarvaM n = replicate n (S.Note (Solkattu.Space Solkattu.Sarva))

-- * by FMatra

dropM, dropM_ :: (CallStack.Stack, Pretty sollu) =>
    FMatra -> SequenceT sollu -> SequenceT sollu
dropM matras = snd . splitM matras
dropM_ matras = snd . splitM_ matras

takeM :: (CallStack.Stack, Pretty sollu) => FMatra -> SequenceT sollu
    -> SequenceT sollu
takeM matras = fst . splitM matras

-- | Like 'splitM_', but mark the sequences as groups.  This way they remember
-- the sollus which were dropped, and realize using the complete sequence, not
-- the fragment.
--
-- TODO the class constraints are unnecessary, but if I want to verify eagerly
-- I'll need them back.
splitM :: (CallStack.Stack, Pretty sollu) => FMatra -> SequenceT sollu
    -> (SequenceT sollu, SequenceT sollu)
splitM matras seq =
    ( groupOf matras Solkattu.After seq
    , groupOf matras Solkattu.Before seq
    )

-- | Split the sequence at the given FMatra.  Unlike 'splitM', this directly
-- splits the sequence, it doesn't create a group.
splitM_ :: (CallStack.Stack, Pretty sollu) => FMatra -> SequenceT sollu
    -> (SequenceT sollu, SequenceT sollu)
splitM_ matras = Solkattu.check . splitM_either matras

splitM_either :: Pretty sollu => FMatra -> SequenceT sollu
    -> Either Text (SequenceT sollu, SequenceT sollu)
splitM_either matras =
    fmap ((S.simplify *** S.simplify) . snd) . go S.defaultTempo matras
    where
    -- Return (matrasLeft, (pre, post)).  matrasLeft is so that a recursive
    -- split in a S.TempoChange or S.Group can report how many matras it
    -- consumed.
    go _ _ [] = Right (0, ([], []))
    go tempo matras (n:ns)
        | matras <= 0 = Right (0, ([], n:ns))
        | noteMatras <= matras =
            second (first (n:)) <$> go tempo (matras - noteMatras) ns
        | otherwise = case n of
            S.TempoChange change subs ->
                group (S.TempoChange change) (S.changeTempo change tempo)
                    matras subs ns
            -- The group is destroyed if it gets split.
            S.Group g children -> do
                (pre, post) <- splitM_either (Solkattu._split g) children
                case Solkattu._side g of
                    Solkattu.Before -> go tempo matras (post ++ ns)
                    Solkattu.After -> go tempo matras (pre ++ ns)
            S.Note (Solkattu.Space space) -> do
                pre <- spaces tempo space matras
                post <- spaces tempo space (noteMatras - matras)
                return (0, (pre, post <> ns))
            _ -> Left $ "can't split a note: " <> pretty matras
                <> " of " <> pretty noteMatras <> ": " <> pretty n
        where
        noteMatras = Solkattu.matrasOf tempo [n]
    group makeGroup tempo matras subs remaining = do
        (left, (pre, post)) <- go tempo matras subs
        if left <= 0
            then Right (0, (make pre, make post ++ remaining))
            else second (first (make subs ++)) <$> go tempo left remaining
        where
        make [] = []
        make ns = [makeGroup ns]

    spaces tempo space matras = do
        speeds <- S.decomposeM matras
        return $ concatMap make speeds
        where
        make s = speed (s - S._speed tempo) [S.Note (Solkattu.Space space)]

rdropM, rdropM_ :: (CallStack.Stack, Pretty sollu) =>
    FMatra -> SequenceT sollu -> SequenceT sollu
rdropM matras seq = takeM (matrasOf seq - matras) seq
rdropM_ matras seq = fst $ splitM_ (matrasOf seq - matras) seq

rtakeM :: (CallStack.Stack, Pretty sollu) => FMatra -> SequenceT sollu
    -> SequenceT sollu
rtakeM dur seq = dropM (matrasOf seq - dur) seq

spaceM :: CallStack.Stack => Solkattu.Space -> FMatra -> SequenceT sollu
spaceM space matras =
    concatMap make $ Solkattu.check $ S.decomposeM matras
    where make s = speed s [S.Note (Solkattu.Space space)]

-- * by Duration

restD, sarvaD :: CallStack.Stack => Duration -> SequenceT sollu
restD = spaceD Solkattu.Rest S.defaultTempo
sarvaD = spaceD Solkattu.Sarva S.defaultTempo

spaceD :: CallStack.Stack => Solkattu.Space -> S.Tempo -> Duration
    -> SequenceT sollu
spaceD space tempo dur =
    concatMap make $ Solkattu.check $ S.decompose s0_matras
    where
    make s = speed (s - S._speed tempo) [S.Note (Solkattu.Space space)]
    -- Cancel out the nadai.  So d is now in s0 matras.
    s0_matras = dur * fromIntegral (S._nadai tempo)

-- | Duration-using variants of the matra functions.  These are only valid
-- at the top level, in 'S.defaultTempo'.  TODO require Tempo arg?
dropD, rdropD, takeD, rtakeD :: (CallStack.Stack, Pretty sollu) =>
    Duration -> SequenceT sollu -> SequenceT sollu
dropD = dropM . dToM
rdropD = rdropM . dToM
takeD = takeM . dToM
rtakeD = rtakeM . dToM

-- * structures

-- | Drop sollus equal in length to some others.  This is intenedd for
-- repeated sequences that get elided away, e.g. @tri p7 . sandi p7 (p7.p6.p5)@.
--
-- I considered an annotation that automatically drops stuff from before which
-- matches stuff afterwards, but it seemed more complicated and less reliable
-- than just dropping explicitly.
sandi :: (CallStack.Stack, Pretty sollu) => SequenceT sollu -> SequenceT sollu
    -> SequenceT sollu
sandi dropped = dropM (matrasOf dropped)

-- | Repeat thrice, with no karvai.
tri :: SequenceT sollu -> SequenceT sollu
tri = tri_ mempty

-- | Repeat thrice, with the given separator.  The _m variant doesn't
-- add the 'mid' tag, which is useful for nested calls.
tri_, tri_m :: SequenceT sollu -> SequenceT sollu -> SequenceT sollu
tri_ sep a = a <> sep <> a <> trySetTag mid sep <> a
tri_m sep a = a <> sep <> a <> sep <> a

-- | Three different patterns with the same separator.
trin :: SequenceT sollu -> SequenceT sollu -> SequenceT sollu
    -> SequenceT sollu -> SequenceT sollu
trin sep a b c = a <> sep <> b <> trySetTag mid sep <> c

-- | Tirmanams with a variant final repeat.
tri2 :: SequenceT sollu -> SequenceT sollu -> SequenceT sollu -> SequenceT sollu
tri2 sep ab c = ab <> sep <> ab <> trySetTag mid sep <> c

-- * sequences

-- | replicate + mconcat.
repeat :: Monoid a => Int -> a -> a
repeat n p = mconcat (replicate n p)

join :: SequenceT sollu -> [SequenceT sollu] -> SequenceT sollu
join = List.intercalate

-- | Intersperse between each stroke.
inter :: SequenceT sollu -> SequenceT sollu -> SequenceT sollu
inter _ [] = []
inter sep (x:xs) = x : sep ++ inter sep xs

spread :: S.Matra -> SequenceT sollu -> SequenceT sollu
spread n = inter (__n n)

cmap :: Monoid b => (a -> b) -> [a] -> b
cmap = mconcatMap

for :: [a] -> (a -> b) -> [b]
for = flip map

cfor :: Monoid b => [a] -> (a -> b) -> b
cfor xs f = mconcatMap f xs

-- | Multiple prefixes on a single suffix.
prefixes :: (Semigroup a, Monoid a) => [a] -> a -> a
prefixes prefs suffix = mconcatMap (<>suffix) prefs

suffixes :: (Semigroup a, Monoid a) => a -> [a] -> a
suffixes prefix sufs = mconcatMap (prefix<>) sufs

circum :: (Semigroup a, Monoid a) => a -> [a] -> a -> a
circum prefix mids suffix = mconcatMap (\m -> prefix <> m <> suffix) mids

suffix :: (Semigroup a, Monoid a) => [a] -> a -> a
suffix seqs suf = mconcat $ map (<>suf) seqs

prefix :: (Semigroup a, Monoid a) => a -> [a] -> a
prefix pref seqs = mconcat $ map (pref<>) seqs

-- | Succesively accumulate suffixes.
accumulate :: Monoid a => [a] -> [a]
accumulate = map mconcat . drop 1 . List.inits

-- * combinators

-- | Reduce three times, with a separator.
reduce3 :: Pretty sollu => FMatra -> SequenceT sollu -> SequenceT sollu
    -> SequenceT sollu
reduce3 dur sep = List.intercalate sep . take 3 . reduceToL dur dur

-- | 'reduceToL', except mconcat the result.
reduceTo :: (CallStack.Stack, Pretty sollu) => FMatra -> FMatra
    -> SequenceT sollu -> SequenceT sollu
reduceTo to by = mconcat . reduceToL to by

-- | Reduce by a duration until a final duration.
reduceToL :: (CallStack.Stack, Pretty sollu) => FMatra -> FMatra
    -> SequenceT sollu -> [SequenceT sollu]
reduceToL to by seq = [dropM m seq | m <- Seq.range 0 (matras - to) by]
    where matras = matrasOf seq

-- | Like 'reduceToL', but drop from the end instead of the front.
reduceToR :: (CallStack.Stack, Pretty sollu) => FMatra -> FMatra
    -> SequenceT sollu -> [SequenceT sollu]
reduceToR to by seq = [takeM m seq | m <- Seq.range matras to (-by)]
    where matras = matrasOf seq

-- | Start fully reduced, and expand n times by the given duration.
expand :: (CallStack.Stack, Pretty sollu) => Int -> FMatra
    -> SequenceT sollu -> [SequenceT sollu]
expand times dur = reverse . take times . reduceToL dur dur

-- | Unlike most other functions, this one doesn't make groups from the reduced
-- sequence.  Since these are used to construct a new sequence, it seems more
-- confusing than helpful.
replaceStart, replaceEnd :: (CallStack.Stack, Pretty sollu) => SequenceT sollu
    -> SequenceT sollu -> SequenceT sollu
replaceStart prefix seq = prefix <> dropM_ (matrasOf prefix) seq
replaceEnd seq suffix = rdropM_ (matrasOf suffix) seq <> suffix

-- * measurement

matrasOf :: SequenceT sollu -> FMatra
matrasOf = Solkattu.matrasOf S.defaultTempo

-- | Like 'matrasOf', but throw an error if it's not integral.
matrasOfI :: CallStack.Stack => SequenceT sollu -> S.Matra
matrasOfI seq
    | frac == 0 = matras
    | otherwise = Solkattu.throw $ "non-integral matras: " <> pretty fmatras
    where
    (matras, frac) = properFraction fmatras
    fmatras = matrasOf seq

-- | I think defaultTempo is ok because these functions are used on fragments.
matraDuration :: S.Duration
matraDuration = S.matraDuration S.defaultTempo

dToM :: Duration -> FMatra
dToM d = realToFrac $ d / S.matraDuration S.defaultTempo

-- * generic notation

-- | Set relative speed.
speed :: S.Speed -> [S.Note g sollu] -> [S.Note g sollu]
speed _ [] = []
speed change seq
    | change == 0 = seq
    | otherwise = [S.TempoChange (S.ChangeSpeed change) seq]

-- | Mnemonic: speed up, slow down.
su, sd :: [S.Note g sollu] -> [S.Note g sollu]
su = speed 1
sd = speed (-1)

nadai :: S.Matra -> [S.Note g sollu] -> [S.Note g sollu]
nadai _ [] = []
nadai n seq = [S.TempoChange (S.Nadai n) seq]

stride :: S.Stride -> [S.Note g sollu] -> [S.Note g sollu]
stride _ [] = []
stride n seq = [S.TempoChange (S.Stride n) seq]

-- | Just mark a group.  Equivalent to dropM 0.
group :: SequenceT sollu -> SequenceT sollu
group = groupOf 0 Solkattu.Before

groupOf :: FMatra -> Solkattu.Side -> SequenceT sollu -> SequenceT sollu
groupOf dropped side = (:[]) . S.Group (Solkattu.Group dropped side)

-- ** tags

-- | Infix operator to 'Solkattu.Tag' all of the sollus it applies to.
(^) :: Solkattu.Tag -> SequenceT sollu -> SequenceT sollu
(^) = setTag
infix 9 ^

-- | 'Solkattu.Middle'.
mid :: Solkattu.Tag
mid = Solkattu.Middle

setTag :: Solkattu.Tag -> SequenceT sollu -> SequenceT sollu
setTag tag = fmap $ fmap $ Solkattu.modifyNote $
    \note -> note { Solkattu._tag = Just tag }

-- | Set if not already set.
trySetTag :: Solkattu.Tag -> SequenceT sollu -> SequenceT sollu
trySetTag tag = fmap $ fmap $ Solkattu.modifyNote $
    \note -> if Solkattu._tag note == Nothing
        then note { Solkattu._tag = Just tag }
        else note

-- * align

-- | Align to the end of the avartanam, with rests.
--
-- This should only be used at the top level, since it gets the timing wrong
-- under a tempo change.
__sam :: (CallStack.Stack, Pretty sollu) =>
    Tala.Tala -> SequenceT sollu -> SequenceT sollu
__sam tala seq = __a (nextSam tala seq) seq

nextSam :: Tala.Tala -> SequenceT sollu -> S.Duration
nextSam tala seq = fromIntegral $ Num.roundUp aksharas dur
    where
    dur = Solkattu.durationOf S.defaultTempo seq
    aksharas = Tala.tala_aksharas tala

-- | Align to the end of the given number of aksharams.
__a :: (CallStack.Stack, Pretty sollu) =>
    S.Duration -> SequenceT sollu -> SequenceT sollu
__a dur seq = replaceEnd (restD dur) seq

sarvaSam :: (CallStack.Stack, Pretty sollu) =>
    Tala.Tala -> SequenceT sollu -> SequenceT sollu
sarvaSam tala seq = sarvaA (nextSam tala seq) seq

sarvaA :: (CallStack.Stack, Pretty sollu) =>
    S.Duration -> SequenceT sollu -> SequenceT sollu
sarvaA dur seq = replaceEnd (sarvaD dur) seq
