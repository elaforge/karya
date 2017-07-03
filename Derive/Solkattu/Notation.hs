-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Generic combinators for solkattu patterns.  Because these are expected to
-- be called as part of the dsl, error calls are allowed.
module Derive.Solkattu.Notation where
import qualified Prelude
import Prelude hiding (reverse)
import qualified Data.List as List

import qualified Util.CallStack as CallStack
import qualified Util.Num as Num
import qualified Derive.Solkattu.Sequence as S
import Derive.Solkattu.Sequence (Duration, Matra)
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


-- Unlike everywhere else except some Utils, I use camelCase in here.  Since
-- this is for a DSL, I try to save horizontal space.

type Sequence stroke = [S.Note (Solkattu.Note stroke)]

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
        | S.Note Solkattu.Rest <- n =
            (0, (restD tempo dur, restD tempo (ndur - dur) <> ns))
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

restD :: CallStack.Stack => S.Tempo -> Duration -> Sequence stroke
restD tempo dur = concatMap generate $ decompose s0_matras
    where
    generate s = speed (s - S.speed tempo) [S.Note Solkattu.Rest]
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

matrasOf :: (CallStack.Stack, Pretty stroke) => Sequence stroke -> Matra
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

-- * combinators

-- | Reduce three times, with a separator.
reduce3 :: Matra -> Sequence stroke -> Sequence stroke -> Sequence stroke
reduce3 n sep = List.intercalate sep . take 3 . iterate (dropM n)

-- | Reduce by a duration until a final duration.
reduceTo :: (CallStack.Stack, Pretty stroke) => Matra -> Matra
    -> Sequence stroke -> Sequence stroke
reduceTo to by seq
    | (matrasOf seq - to) `mod` by /= 0 =
        errorStack $ showt (matrasOf seq) <> " can't reduce by "
            <> showt by <> " to " <> showt to
    | otherwise = mconcat $ takeWhile ((>=to) . matrasOf) $
        iterate (dropM by) seq

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
__sam :: CallStack.Stack => Tala.Tala -> Sequence stroke -> Sequence stroke
__sam tala seq = __a (fromIntegral end) seq
    where
    dur = Solkattu.duration_of seq
    aksharas = sum (Tala.tala_aksharas tala)
    end = Num.roundUp aksharas dur

-- | Align to the end of the given number of aksharams.
__a :: CallStack.Stack => S.Duration -> Sequence stroke -> Sequence stroke
__a dur seq = replaceEnd (restD S.default_tempo dur) seq
