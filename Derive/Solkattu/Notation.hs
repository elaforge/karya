-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Generic combinators for solkattu patterns.  Because these are expected to
-- be called as part of the dsl, error calls are allowed.
--
-- This is meant to have just Sequence manipulation, without
-- instrument-specific functions.
module Derive.Solkattu.Notation where
import qualified Prelude
import Prelude hiding (repeat, reverse)
import qualified Data.List as List

import qualified Util.CallStack as CallStack
import qualified Util.Num as Num
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as S
import Derive.Solkattu.Sequence (Duration, Matra)
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


-- Unlike everywhere else except some Utils, I use camelCase in here.  Since
-- this is for a DSL, I try to save horizontal space.

type Sequence stroke = [S.Note (Solkattu.Note stroke)]

-- * rests

class Rest a where __ :: a
instance Rest (Sequence stroke) where
    __ = [S.Note (Solkattu.Space Solkattu.Rest)]
instance Rest (Realize.SNote stroke) where
    __ = Realize.rest

-- | These are meant to suffix a sollu.  Since the sollu is considered part of
-- the duration, the number is one higher than the number of rests.  E.g.
-- @din.__3@ is a 3 count, and equivalent to @din.__.__@.
__2, __3, __4, __5, __6, __7, __8, __9 :: Sequence stroke
__2 = __
__3 = __n 3
__4 = __n 4
__5 = __n 5
__6 = __n 6
__7 = __n 7
__8 = __n 8
__9 = __n 9

__n :: Matra -> Sequence stroke
__n n = repeat (n-1) __

-- * by Duration

dropD :: CallStack.Stack => Duration -> Sequence stroke -> Sequence stroke
dropD dur = snd . splitD dur

takeD :: CallStack.Stack => Duration -> Sequence stroke -> Sequence stroke
takeD dur = fst . splitD dur

splitD :: CallStack.Stack => Duration -> Sequence stroke
    -> (Sequence stroke, Sequence stroke)
splitD dur = (S.simplify *** S.simplify) .  snd . go S.default_tempo dur
    where
    go _ _ [] = (0, ([], []))
    go tempo dur (n:ns)
        | dur <= 0 = (0, ([], n:ns))
        | ndur <= dur = second (first (n:)) $ go tempo (dur - ndur) ns
        | S.TempoChange change subs <- n = tempo_change tempo dur change subs ns
        | S.Note (Solkattu.Space space) <- n =
            (0, (spaceD space tempo dur, spaceD space tempo (ndur - dur) <> ns))
        -- TODO drop a Pattern, replace with rests
        -- or just error
        | otherwise = errorStack $ "can't split a note: " <> pretty (dur / ndur)
        where ndur = S.note_duration tempo n
    tempo_change tempo dur change subs ns
        | dur_left <= 0 =
            (dur_left, (make_tempo sub_pre, make_tempo sub_post ++ ns))
        | otherwise = case go tempo dur_left ns of
            (end_dur, (pre, post)) ->
                (end_dur, (make_tempo sub_post ++ pre, post))
        where
        (dur_left, (sub_pre, sub_post)) =
            go (S.change_tempo change tempo) dur subs
        make_tempo [] = []
        make_tempo ns = [S.TempoChange change ns]

rdropD :: Duration -> Sequence stroke -> Sequence stroke
rdropD dur = reverse . dropD dur . reverse

rtakeD :: Duration -> Sequence stroke -> Sequence stroke
rtakeD dur = reverse . takeD dur . reverse

restD, sarvaD :: CallStack.Stack => Duration -> Sequence stroke
restD = spaceD Solkattu.Rest S.default_tempo
sarvaD = spaceD Solkattu.Sarva S.default_tempo

spaceD :: CallStack.Stack => Solkattu.Space -> S.Tempo -> Duration
    -> Sequence stroke
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
        | speed > 4 = errorStack $ "not a binary multiple: " <> pretty dur
        | matra <= left = speed : go (speed+1) (left - matra)
        | otherwise = go (speed+1) left
        -- where factor = S.speed_factor speed
        where matra = 1 / S.speed_factor speed

reverse :: [S.Note a] -> [S.Note a]
reverse = map sub . Prelude.reverse
    where
    sub (S.TempoChange change subs) = S.TempoChange change (reverse subs)
    sub note@(S.Note _) = note

-- * by Matra

-- | Drop a number of matras from the Sequence.
dropM :: Matra -> Sequence stroke -> Sequence stroke
dropM matras = dropD (fromIntegral matras * matra_duration)

rdropM :: Matra -> Sequence stroke -> Sequence stroke
rdropM matras = reverse . dropM matras . reverse

takeM :: Matra -> Sequence stroke -> Sequence stroke
takeM matras = takeD (fromIntegral matras * matra_duration)

rtakeM :: Matra -> Sequence stroke -> Sequence stroke
rtakeM matras = reverse . takeM matras . reverse

matrasOf :: CallStack.Stack => Sequence stroke -> Matra
matrasOf = Solkattu.check . matrasOfE

-- | Get the number of sollu-matras.  Whether or not this corresponds to
-- tala matras depends on the speed.
matrasOfE :: Sequence a -> Either Text S.Matra
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

-- * structures

-- | Repeat thrice, with no karvai.
tri :: Sequence stroke -> Sequence stroke
tri = tri_ mempty

-- | Repeat thrice, with the given separator.
tri_ :: Sequence stroke -> Sequence stroke -> Sequence stroke
tri_ sep seq = join sep [seq, seq, seq]

-- | Three different patterns with the same separator.
trin :: Sequence stroke -> Sequence stroke -> Sequence stroke
    -> Sequence stroke -> Sequence stroke
trin sep a b c = join sep [a, b, c]

-- | Tirmanams with a variant final repeat.
tri2 :: Sequence stroke -> Sequence stroke -> Sequence stroke -> Sequence stroke
tri2 sep ab c = join sep [ab, ab, c]

-- * sequences

-- | replicate + mconcat.
repeat :: Monoid a => Int -> a -> a
repeat n p = mconcat (replicate n p)

join :: Sequence stroke -> [Sequence stroke] -> Sequence stroke
join = List.intercalate

-- | Intersperse between each stroke.
inter :: Sequence stroke -> Sequence stroke -> Sequence stroke
inter _ [] = []
inter sep (x:xs) = x : sep ++ inter sep xs

spread :: Matra -> Sequence stroke -> Sequence stroke
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
reduce3 :: Matra -> Sequence stroke -> Sequence stroke -> Sequence stroke
reduce3 n sep = List.intercalate sep . take 3 . iterate (dropM n)

-- | 'reduceToL', except mconcat the result.
reduceTo :: CallStack.Stack => Matra -> Matra -> Sequence stroke
    -> Sequence stroke
reduceTo to by = mconcat . reduceToL to by

-- | Reduce by a duration until a final duration.
reduceToL :: CallStack.Stack => Matra -> Matra -> Sequence stroke
    -> [Sequence stroke]
reduceToL to by seq
    | (matrasOf seq - to) `mod` by /= 0 =
        errorStack $ showt (matrasOf seq) <> " can't reduce by "
            <> showt by <> " to " <> showt to
    | otherwise = takeWhile ((>=to) . matrasOf) $ iterate (dropM by) seq

-- | Reduce by dropping the end.
reduceR :: Matra -> Sequence stroke -> [Sequence stroke]
reduceR n = iterate (rdropM n)

reduceR3 :: Matra -> Sequence stroke -> Sequence stroke -> Sequence stroke
reduceR3 dur sep = List.intercalate sep . take 3 . reduceR dur

-- | Start fully reduced, and expand n times by the given duration.
expand :: Int -> Matra -> Sequence stroke -> [Sequence stroke]
expand times dur = Prelude.reverse . take times . iterate (dropM dur)

replaceEnd :: CallStack.Stack => Sequence stroke -> Sequence stroke
    -> Sequence stroke
replaceEnd seq suffix = rdropD (Solkattu.duration_of suffix) seq <> suffix

replaceStart :: CallStack.Stack => Sequence stroke -> Sequence stroke
    -> Sequence stroke
replaceStart prefix seq = prefix <> dropD (Solkattu.duration_of prefix) seq

-- | Set relative speed.
speed :: S.Speed -> [S.Note stroke] -> [S.Note stroke]
speed _ [] = []
speed change seq
    | change == 0 = seq
    | otherwise = [S.TempoChange (S.ChangeSpeed change) seq]

-- | Mnemonic: speed up, slow down.
su, sd :: [S.Note stroke] -> [S.Note stroke]
su = speed 1
sd = speed (-1)

nadai :: Matra -> [S.Note stroke] -> [S.Note stroke]
nadai _ [] = []
nadai n seq = [S.TempoChange (S.Nadai n) seq]

-- | I think default_tempo is ok because these functions are used on fragments.
matra_duration :: S.Duration
matra_duration = S.matra_duration S.default_tempo

-- * align

-- | Align to the end of the avartanam, with rests.
--
-- This should only be used at the top level, since it gets the timing wrong
-- under a tempo change.
__sam :: CallStack.Stack => Tala.Tala -> Sequence stroke -> Sequence stroke
__sam tala seq = __a (next_sam tala seq) seq

next_sam :: Tala.Tala -> Sequence stroke -> S.Duration
next_sam tala seq = fromIntegral $ Num.roundUp aksharas dur
    where
    dur = Solkattu.duration_of seq
    aksharas = sum (Tala.tala_aksharas tala)

-- | Align to the end of the given number of aksharams.
__a :: CallStack.Stack => S.Duration -> Sequence stroke -> Sequence stroke
__a dur seq = replaceEnd (restD dur) seq

sarvaSam :: CallStack.Stack => Tala.Tala -> Sequence stroke -> Sequence stroke
sarvaSam tala seq = sarvaA (next_sam tala seq) seq

sarvaA :: CallStack.Stack => S.Duration -> Sequence stroke -> Sequence stroke
sarvaA dur seq = replaceEnd (sarvaD dur) seq
