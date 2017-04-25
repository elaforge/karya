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
import qualified Util.Pretty as Pretty

import qualified Derive.Solkattu.Sequence as S
import Derive.Solkattu.Sequence (Duration, Matra)
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Solkattu (Note)
import qualified Derive.Solkattu.Tala as Tala

import Global


type Sequence a = [Note a]

-- * by Duration

dropD :: CallStack.Stack => Duration -> Sequence stroke -> Sequence stroke
dropD dur = snd . splitD dur

takeD :: CallStack.Stack => Duration -> Sequence stroke -> Sequence stroke
takeD dur = fst . splitD dur

splitD :: CallStack.Stack => Duration -> Sequence stroke
    -> (Sequence stroke, Sequence stroke)
splitD dur = snd . go S.default_tempo dur
    where
    go _ _ [] = (0, ([], []))
    go tempo dur (n:ns)
        | dur <= 0 = (0, ([], n:ns))
        | ndur <= dur = second (first (n:)) $ go tempo (dur - ndur) ns
        | S.TempoChange change subs <- n = tempo_change tempo dur change subs ns
        -- TODO drop a Pattern, replace with rests
        -- or just error
        | otherwise = errorStack $
            "can't split on a fractional duration: " <> pretty dur
        where ndur = S.note_duration Solkattu.note_matras tempo n
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

matras_of :: (CallStack.Stack, Pretty.Pretty stroke) => Sequence stroke -> Matra
matras_of = Solkattu.check . matras_of_e

-- | Get the number of sollu-matras.  Whether or not this corresponds to
-- tala matras depends on the speed.
matras_of_e :: [Note a] -> Either Text S.Matra
matras_of_e = integral <=< justErr "nadai change" . of_sequence
    where
    integral dur
        | frac == 0 = Right matras
        | otherwise = Left "non-integral matras"
        where (matras, frac) = properFraction dur
    -- If the whole thing is in a certain nadai that's not really a change.
    of_sequence [S.TempoChange (S.Nadai _) notes] = of_sequence notes
    of_sequence notes = sum <$> traverse of_note notes
    of_note (S.Note note) = Just $ fromIntegral $ Solkattu.note_matras note
    of_note (S.TempoChange change notes) = case change of
        S.ChangeSpeed speed -> (/ S.speed_factor speed) <$> of_sequence notes
        S.Nadai _ -> Nothing

-- * combinators

-- | Reduce three times, with a separator.
reduce3 :: Matra -> Sequence stroke -> Sequence stroke -> Sequence stroke
reduce3 n sep = List.intercalate sep . take 3 . iterate (dropM n)

-- | Reduce by a duration until a final duration.
reduceTo :: (CallStack.Stack, Pretty.Pretty stroke) => Matra -> Matra
    -> Sequence stroke -> Sequence stroke
reduceTo by to seq
    | (matras_of seq - to) `mod` by /= 0 =
        errorStack $ showt (matras_of seq) <> " can't reduce by "
            <> showt by <> " to " <> showt to
    | otherwise = mconcat $ takeWhile ((>=to) . matras_of) $
        iterate (dropM by) seq

-- | Reduce by dropping the end.
reduceR :: Matra -> Sequence stroke -> [Sequence stroke]
reduceR n = iterate (rdropM n)

reduceR3 :: Matra -> Sequence stroke -> Sequence stroke -> Sequence stroke
reduceR3 dur sep = List.intercalate sep . take 3 . reduceR dur

-- | Start fully reduced, and expand to the given sequence.
expand :: Int -> Matra -> Sequence stroke -> [Sequence stroke]
expand times dur = Prelude.reverse . take times . iterate (dropM dur)

replaceEnd :: Sequence stroke -> Sequence stroke -> Sequence stroke
replaceEnd seq suffix = rdropD (Solkattu.duration_of suffix) seq <> suffix

replaceStart :: Sequence stroke -> Sequence stroke -> Sequence stroke
replaceStart prefix seq = prefix <> dropD (Solkattu.duration_of prefix) seq

-- | Increase speed by a multiple by incrementing Speeds.
speed :: S.Speed -> [S.Note stroke] -> [S.Note stroke]
speed _ [] = []
speed change seq = [S.TempoChange (S.ChangeSpeed change) seq]

faster :: [S.Note stroke] -> [S.Note stroke]
faster = speed 1

slower :: [S.Note stroke] -> [S.Note stroke]
slower = speed (-1)

nadai :: Matra -> [S.Note stroke] -> [S.Note stroke]
nadai _ [] = []
nadai n seq = [S.TempoChange (S.Nadai n) seq]

-- | I think default_tempo is ok because these functions are used on fragments.
matra_duration :: S.Duration
matra_duration = S.matra_duration S.default_tempo

-- * align

-- | Align to the end of the avartanam.
align :: CallStack.Stack => Tala.Tala -> Sequence stroke -> Sequence stroke
align tala seq = restD nadai (fromIntegral end - dur) <> seq
    where
    nadai = S.nadai S.default_tempo
    dur = Solkattu.duration_of seq
    end = Num.roundUp aksharas dur
    aksharas = sum (Tala.tala_aksharas tala)

-- | Rest for a certain Duration.
restD :: CallStack.Stack => S.Nadai -> Duration -> Sequence stroke
restD nadai aksharas
    | length matras == 4 =
        errorStack $ pretty aksharas <> " aksharas can't be represented at s2"
    | otherwise = rest matras
    where
    rest (m:ms) = map S.Note (replicate m Solkattu.Rest) <> faster (rest ms)
    rest [] = mempty
    matras = take 4 $ decompose (aksharas * fromIntegral nadai)
    decompose :: Duration -> [Matra]
    decompose dur = int : if frac == 0 then [] else decompose (frac * 2)
        where (int, frac) = properFraction dur
