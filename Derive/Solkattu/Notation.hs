-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Generic combinators for solkattu patterns.  Because these are expected to
-- be called as part of the dsl, error calls are allowed.
module Derive.Solkattu.Notation where
import qualified Data.List as List

import qualified Util.CallStack as CallStack
import qualified Util.Pretty as Pretty
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Solkattu (Note)
import qualified Derive.Solkattu.Sequence as S
import Derive.Solkattu.Sequence (Duration, Matra)
import Global


type Sequence a = [Note a]

-- | Drop a number of matras from the Sequence.
dropM :: Matra -> Sequence stroke -> Sequence stroke
dropM matras = dropD (fromIntegral matras * matra_duration)

rdropM :: Matra -> Sequence stroke -> Sequence stroke
rdropM matras = reverse . dropM matras . reverse

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
        | S.TempoChange change subs <- n =
            tempo_change tempo dur change subs ns
        -- TODO drop a Pattern, replace with rests
        -- or just error
        | otherwise = errorStack $
            "can't split on a fractional duration: " <> pretty dur
        where ndur = S.note_duration Solkattu.note_matras tempo n
    tempo_change tempo dur change subs ns
        | dur_left <= 0 = (dur_left, ([S.TempoChange change sub_pre], []))
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

takeM :: Matra -> Sequence stroke -> Sequence stroke
takeM matras = takeD (fromIntegral matras * matra_duration)

rtakeM :: Matra -> Sequence stroke -> Sequence stroke
rtakeM matras = reverse . takeM matras . reverse

rtakeD :: Duration -> Sequence stroke -> Sequence stroke
rtakeD dur = reverse . takeD dur . reverse

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

matras_of :: (CallStack.Stack, Pretty.Pretty stroke) =>
    Sequence stroke -> Matra
matras_of = either errorStack id . Solkattu.matras_of

-- | Reduce by dropping the end.
reduceR :: Matra -> Sequence stroke -> [Sequence stroke]
reduceR n = iterate (rdropM n)

reduceR3 :: Matra -> Sequence stroke -> Sequence stroke -> Sequence stroke
reduceR3 dur sep = List.intercalate sep . take 3 . reduceR dur

-- | Start fully reduced, and expand to the given sequence.
expand :: Int -> Matra -> Sequence stroke -> [Sequence stroke]
expand times dur = reverse . take times . iterate (dropM dur)

replaceEnd :: Sequence stroke -> Sequence stroke -> Sequence stroke
replaceEnd seq suffix = rdropD (Solkattu.duration_of suffix) seq <> suffix

replaceStart :: Sequence stroke -> Sequence stroke -> Sequence stroke
replaceStart prefix seq = prefix <> dropD (Solkattu.duration_of prefix) seq

-- | Increase speed by a multiple by incrementing Speeds.
speed :: S.Speed -> [S.Note stroke] -> [S.Note stroke]
speed change seq = [S.TempoChange (S.SpeedChange change) seq]

faster :: [S.Note stroke] -> [S.Note stroke]
faster = speed 1

slower :: [S.Note stroke] -> [S.Note stroke]
slower = speed (-1)

nadai :: Matra -> [S.Note stroke] -> [S.Note stroke]
nadai n seq = [S.TempoChange (S.Nadai n) seq]

-- | I think default_tempo is ok because these functions are used on fragments.
matra_duration :: S.Duration
matra_duration = S.matra_duration S.default_tempo
