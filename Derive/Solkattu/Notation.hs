-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Generic combinators for solkattu patterns.  Because these are expected to
-- be called as part of the dsl, impure exceptions are allowed, via
-- 'Solkattu.throw'.
--
-- This is meant to have just Sequence manipulation, without
-- instrument-specific functions.
module Derive.Solkattu.Notation where
import Prelude hiding ((^), repeat)
import qualified Data.List as List

import qualified Util.CallStack as CallStack
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as S
import Derive.Solkattu.Sequence (Duration, Matra)
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


-- Unlike everywhere else except some Utils, I use camelCase in here.  Since
-- this is for a DSL, I try to save horizontal space.

type SequenceT sollu = [S.Note (Solkattu.Group sollu) (Solkattu.Note sollu)]
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

__n :: Matra -> SequenceT sollu
__n n = repeat (n-1) __

restM :: Matra -> SequenceT sollu
restM n = repeat n __

sarvaM :: Matra -> SequenceT sollu
sarvaM n = replicate n (S.Note (Solkattu.Space Solkattu.Sarva))

-- * by Duration

dropD, dropD_ :: (CallStack.Stack, Pretty sollu) => Duration
    -> SequenceT sollu -> SequenceT sollu
dropD dur = snd . splitD dur
dropD_ dur = snd . splitD_ dur

takeD :: (CallStack.Stack, Pretty sollu) => Duration -> SequenceT sollu
    -> SequenceT sollu
takeD dur = fst . splitD dur

-- | Like 'splitD_', but mark the sequences as groups.  This way they remember
-- the sollus which were dropped, and realize using the complete sequence, not
-- the fragment.
splitD :: (CallStack.Stack, Pretty sollu) => Duration -> SequenceT sollu
    -> (SequenceT sollu, SequenceT sollu)
splitD dur seq =
    ( groupOf (sollus_of post) Solkattu.Back pre
    , groupOf (sollus_of pre) Solkattu.Front post
    )
    where (pre, post) = splitD_ dur seq

-- | Split the sequence at the given Duration.
splitD_ :: (CallStack.Stack, Pretty sollu) => Duration -> SequenceT sollu
    -> (SequenceT sollu, SequenceT sollu)
splitD_ dur = (S.simplify *** S.simplify) .  snd . go S.default_tempo dur
    where
    go _ _ [] = (0, ([], []))
    go tempo dur (n:ns)
        | dur <= 0 = (0, ([], n:ns))
        | ndur <= dur = second (first (n:)) $ go tempo (dur - ndur) ns
        | otherwise = case n of
            S.TempoChange change subs ->
                group (S.TempoChange change) (S.change_tempo change tempo)
                    dur subs ns
            -- TODO actually this is wrong.  I would have to add extra the
            -- split off sollus to the group, but no need to bother until it
            -- becomes a problem.
            S.Group g subs -> group (S.Group g) tempo dur subs ns
            S.Note (Solkattu.Space space) -> (0,
                (spaceD space tempo dur, spaceD space tempo (ndur - dur) <> ns))
            _ -> Solkattu.throw $ "can't split a note: " <> pretty dur
                <> " of " <> pretty ndur <> ": " <> pretty n
        where ndur = S.note_duration tempo n
    group make_group tempo dur subs remaining
        | left <= 0 = (0, (make pre, make post ++ remaining))
        | otherwise = second (first (make subs ++)) (go tempo left remaining)
        where
        (left, (pre, post)) = go tempo dur subs
        make [] = []
        make ns = [make_group ns]

splitS :: Int -> SequenceT sollu -> (SequenceT sollu, SequenceT sollu)
splitS count seq =
    ( groupOf (sollus_of post) Solkattu.Back pre
    , groupOf (sollus_of pre) Solkattu.Front post
    )
    where (pre, post) = splitS_ count seq

splitS_ :: Int -> SequenceT sollu -> (SequenceT sollu, SequenceT sollu)
splitS_ count = (S.simplify *** S.simplify) . snd . go count
    where
    go count ns | count <= 0 = (0, ([], ns))
    go count [] = (count, ([], []))
    go count (n:ns) = case n of
        S.Note {} -> second (first (n:)) (go (count-1) ns)
        S.TempoChange change subs -> group (S.TempoChange change) count subs ns
        -- TODO actually this is wrong.  I would have to add extra the split
        -- off sollus to the group, but no need to bother until it becomes
        -- a problem.
        S.Group g subs -> group (S.Group g) count subs ns
    group make_group count subs remaining
        | left <= 0 = (0, (make pre, make post ++ remaining))
        | otherwise = second (first (make subs ++)) (go left remaining)
        where
        (left, (pre, post)) = go count subs
        make [] = []
        make ns = [make_group ns]

sollus_of :: SequenceT sollu -> [sollu]
sollus_of = mapMaybe Solkattu.sollu_of . S.notes

rdropD, rdropD_ :: (CallStack.Stack, Pretty sollu) => Duration
    -> SequenceT sollu -> SequenceT sollu
rdropD dur seq = takeD (Solkattu.duration_of seq - dur) seq
rdropD_ dur seq = fst $ splitD_ (Solkattu.duration_of seq - dur) seq

rtakeD :: (CallStack.Stack, Pretty sollu) => Duration -> SequenceT sollu
    -> SequenceT sollu
rtakeD dur seq = dropD (Solkattu.duration_of seq - dur) seq

restD, sarvaD :: CallStack.Stack => Duration -> SequenceT sollu
restD = spaceD Solkattu.Rest S.default_tempo
sarvaD = spaceD Solkattu.Sarva S.default_tempo

spaceD :: CallStack.Stack => Solkattu.Space -> S.Tempo -> Duration
    -> SequenceT sollu
spaceD space tempo dur = concatMap generate $ decompose s0_matras
    where
    generate s = speed (s - S.speed tempo) [S.Note (Solkattu.Space space)]
    -- Cancel out the nadai.  So d is now in s0 matras.
    s0_matras = dur * fromIntegral (S.nadai tempo)

-- | Given a duration, return the speeds needed to add up to that duration.
-- Crash if the speed went past 4, which means the duration probably isn't
-- binary.
decompose :: CallStack.Stack => Duration -> [S.Speed]
decompose dur = go (- floor (logBase 2 (realToFrac dur))) dur
    where
    go speed left
        | left == 0 = []
        | speed > 4 = Solkattu.throw $ "not a binary multiple: " <> pretty dur
        | matra <= left = speed : go (speed+1) (left - matra)
        | otherwise = go (speed+1) left
        -- where factor = S.speed_factor speed
        where matra = 1 / S.speed_factor speed

-- * by Matra

-- | Matra-using variants of the duration functions.
dropM, rdropM, takeM, rtakeM :: (CallStack.Stack, Pretty sollu) =>
    Matra -> SequenceT sollu -> SequenceT sollu
dropM = dropD . mToD
rdropM = rdropD . mToD
takeM = takeD . mToD
rtakeM = rtakeD . mToD

matrasOf :: CallStack.Stack => SequenceT sollu -> Matra
matrasOf = Solkattu.check . matrasOfE

-- | Get the number of sollu-matras.  Whether or not this corresponds to
-- tala matras depends on the speed.
matrasOfE :: SequenceT a -> Either Text S.Matra
matrasOfE = integral <=< justErr "nadai change" . of_sequence
    where
    integral dur
        | frac == 0 = Right matras
        | otherwise = Left "non-integral matras"
        where (matras, frac) = properFraction dur
    -- If the whole thing is in a certain nadai that's not really a change.
    of_sequence [S.TempoChange (S.Nadai _) notes] = of_sequence notes
    of_sequence notes = sum <$> traverse of_note notes
    of_note (S.Note note) = Just $ fromIntegral $ S.matras_of note
    of_note (S.TempoChange change notes) = case change of
        S.ChangeSpeed speed -> (/ S.speed_factor speed) <$> of_sequence notes
        S.Nadai _ -> Nothing
        S.Stride stride -> (* fromIntegral stride) <$> of_sequence notes
    of_note (S.Group _ notes) = of_sequence notes

-- * by sollu

takeS, dropS :: Int -> SequenceT sollu -> SequenceT sollu
takeS n = fst . splitS n
dropS n = snd . splitS n

-- * structures

-- | Repeat thrice, with no karvai.
tri :: SequenceT sollu -> SequenceT sollu
tri = tri_ mempty

-- | Repeat thrice, with the given separator.
tri_ :: SequenceT sollu -> SequenceT sollu -> SequenceT sollu
tri_ sep a = a <> sep <> a <> try_set_tag mid sep <> a

-- | Three different patterns with the same separator.
trin :: SequenceT sollu -> SequenceT sollu -> SequenceT sollu
    -> SequenceT sollu -> SequenceT sollu
trin sep a b c = a <> sep <> b <> try_set_tag mid sep <> c

-- | Tirmanams with a variant final repeat.
tri2 :: SequenceT sollu -> SequenceT sollu -> SequenceT sollu -> SequenceT sollu
tri2 sep ab c = ab <> sep <> ab <> try_set_tag mid sep <> c

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

spread :: Matra -> SequenceT sollu -> SequenceT sollu
spread n = inter (__n n)

cmap :: Monoid b => (a -> b) -> [a] -> b
cmap = mconcatMap

for :: [a] -> (a -> b) -> [b]
for = flip map

cfor :: Monoid b => [a] -> (a -> b) -> b
cfor xs f = mconcatMap f xs

-- | Multiple prefixes on a single suffix.
prefixes :: Monoid a => [a] -> a -> a
prefixes prefs suffix = mconcatMap (<>suffix) prefs

suffixes :: Monoid a => a -> [a] -> a
suffixes prefix sufs = mconcatMap (prefix<>) sufs

circum :: Monoid a => a -> [a] -> a -> a
circum prefix mids suffix = mconcatMap (\m -> prefix <> m <> suffix) mids

suffix :: Monoid a => [a] -> a -> a
suffix seqs suf = mconcat $ map (<>suf) seqs

prefix :: Monoid a => a -> [a] -> a
prefix pref seqs = mconcat $ map (pref<>) seqs

-- | Succesively accumulate suffixes.
accumulate :: Monoid a => [a] -> [a]
accumulate = map mconcat . drop 1 . List.inits

-- * combinators

-- | Reduce three times, with a separator.
reduce3 :: Pretty sollu => Matra -> SequenceT sollu -> SequenceT sollu
    -> SequenceT sollu
reduce3 dur sep = List.intercalate sep . take 3 . reduceToL dur dur

-- | 'reduceToL', except mconcat the result.
reduceTo :: (CallStack.Stack, Pretty sollu) => Matra -> Matra
    -> SequenceT sollu -> SequenceT sollu
reduceTo to by = mconcat . reduceToL to by

-- | Reduce by a duration until a final duration.
reduceToL :: (CallStack.Stack, Pretty sollu) => Matra -> Matra
    -> SequenceT sollu -> [SequenceT sollu]
reduceToL to by seq = [dropM m seq | m <- Seq.range 0 (matras - to) by]
    where matras = matrasOf seq

-- | Like 'reduceToL', but drop from the end instead of the front.
reduceToR :: (CallStack.Stack, Pretty sollu) => Matra -> Matra
    -> SequenceT sollu -> [SequenceT sollu]
reduceToR to by seq = [takeM m seq | m <- Seq.range matras to (-by)]
    where matras = matrasOf seq

-- | Start fully reduced, and expand n times by the given duration.
expand :: (CallStack.Stack, Pretty sollu) => Int -> Matra
    -> SequenceT sollu -> [SequenceT sollu]
expand times dur = reverse . take times . reduceToL dur dur

-- | Unlike most other functions, this one doesn't make groups from the reduced
-- sequence.  Since these are used to construct a new sequence, it seems more
-- confusing than helpful.
replaceStart, replaceEnd :: (CallStack.Stack, Pretty sollu) => SequenceT sollu
    -> SequenceT sollu -> SequenceT sollu
replaceStart prefix seq = prefix <> dropD_ (Solkattu.duration_of prefix) seq
replaceEnd seq suffix = rdropD_ (Solkattu.duration_of suffix) seq <> suffix

-- | I think default_tempo is ok because these functions are used on fragments.
matra_duration :: S.Duration
matra_duration = S.matra_duration S.default_tempo

mToD :: Matra -> Duration
mToD = (*matra_duration) . fromIntegral

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

nadai :: Matra -> [S.Note g sollu] -> [S.Note g sollu]
nadai _ [] = []
nadai n seq = [S.TempoChange (S.Nadai n) seq]

stride :: S.Stride -> [S.Note g sollu] -> [S.Note g sollu]
stride _ [] = []
stride n seq = [S.TempoChange (S.Stride n) seq]

-- | Just mark a group.  Equivalent to dropM 0.
group :: SequenceT sollu -> SequenceT sollu
group = groupOf [] Solkattu.Front

groupOf :: [sollu] -> Solkattu.Side -> SequenceT sollu -> SequenceT sollu
groupOf dropped side = (:[]) . S.Group (Solkattu.Group dropped side)

-- ** tags

-- | Infix operator to 'Solkattu.Tag' all of the sollus it applies to.
(^) :: Solkattu.Tag -> SequenceT sollu -> SequenceT sollu
(^) = set_tag
infix 9 ^

mid :: Solkattu.Tag
mid = Solkattu.Middle

set_tag :: Solkattu.Tag -> SequenceT sollu -> SequenceT sollu
set_tag tag = fmap $ fmap $ Solkattu.modify_note $
    \note -> note { Solkattu._tag = Just tag }

-- | Set if not already set.
try_set_tag :: Solkattu.Tag -> SequenceT sollu -> SequenceT sollu
try_set_tag tag = fmap $ fmap $ Solkattu.modify_note $
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
__sam tala seq = __a (next_sam tala seq) seq

next_sam :: Tala.Tala -> SequenceT sollu -> S.Duration
next_sam tala seq = fromIntegral $ Num.roundUp aksharas dur
    where
    dur = Solkattu.duration_of seq
    aksharas = sum (Tala.tala_aksharas tala)

-- | Align to the end of the given number of aksharams.
__a :: (CallStack.Stack, Pretty sollu) =>
    S.Duration -> SequenceT sollu -> SequenceT sollu
__a dur seq = replaceEnd (restD dur) seq

sarvaSam :: (CallStack.Stack, Pretty sollu) =>
    Tala.Tala -> SequenceT sollu -> SequenceT sollu
sarvaSam tala seq = sarvaA (next_sam tala seq) seq

sarvaA :: (CallStack.Stack, Pretty sollu) =>
    S.Duration -> SequenceT sollu -> SequenceT sollu
sarvaA dur seq = replaceEnd (sarvaD dur) seq
