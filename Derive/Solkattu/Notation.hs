-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Generic combinators for solkattu patterns.
module Derive.Solkattu.Notation where
import qualified Data.List as List

import qualified Util.CallStack as CallStack
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Solkattu (Duration, Matras, Sequence)
import Global


-- | Drop a number of matras from the Sequence.  Patterns will be shortened.
dropM :: Matras -> Sequence stroke -> Sequence stroke
dropM matras ns = case ns of
    [] -> []
    (n:ns)
        | matras <= 0 -> (n:ns)
        | otherwise -> case n of
            Solkattu.Pattern dur
                | dur > matras -> Solkattu.Pattern (dur - matras) : ns
                | otherwise -> dropM (matras - dur) ns
            _ -> dropM (matras - Solkattu.note_matras n) ns

rdropM :: Matras -> Sequence stroke -> Sequence stroke
rdropM matras = reverse . dropM matras . reverse

dropD :: Duration -> Sequence stroke -> Sequence stroke
dropD dur ns = go dur (Solkattu.durations_of ns) ns
    where
    go dur _ ns | dur <= 0 = ns
    go dur (d:ds) (_:ns) = go (dur-d) ds ns
    go _ _ _ = []

rdropD :: Duration -> Sequence stroke -> Sequence stroke
rdropD dur = reverse . dropD dur . reverse

takeM :: Matras -> Sequence stroke -> Sequence stroke
takeM _ [] = []
takeM matras _ | matras <= 0 = []
takeM matras (n:ns) = case n of
    Solkattu.Sollu {} -> n : takeM (matras-1) ns
    Solkattu.Rest {} -> n : takeM (matras-1) ns
    Solkattu.Pattern dur
        | dur > matras -> n : takeM (matras-dur) ns
        | otherwise -> [Solkattu.Pattern (dur - matras)]
    Solkattu.Alignment {} -> takeM matras ns
    Solkattu.TimeChange {} -> takeM matras ns

rtakeM :: Matras -> Sequence stroke -> Sequence stroke
rtakeM matras = reverse . takeM matras . reverse

takeD :: Duration -> Sequence stroke -> Sequence stroke
takeD dur ns = go dur (Solkattu.durations_of ns) ns
    where
    go dur _ _ | dur <= 0 = []
    go dur (d:ds) (n:ns) = n : go (dur-d) ds ns
    go _ _ _ = []

rtakeD :: Duration -> Sequence stroke -> Sequence stroke
rtakeD dur = reverse . takeD dur . reverse

-- | Reduce three times, with a separator.
reduce3 :: Matras -> Sequence stroke -> Sequence stroke -> Sequence stroke
reduce3 n sep = List.intercalate sep . take 3 . iterate (dropM n)

-- | Reduce by a duration until a final duration.
reduceTo :: CallStack.Stack => Matras -> Matras -> Sequence stroke
    -> Sequence stroke
reduceTo by to seq
    | (Solkattu.matras_of seq - to) `mod` by /= 0 =
        errorStack $ showt (Solkattu.matras_of seq) <> " can't reduce by "
            <> showt by <> " to " <> showt to
    | otherwise = mconcat $ takeWhile ((>=to) . Solkattu.matras_of) $
        iterate (dropM by) seq

-- | Reduce by dropping the end.
reduceR :: Matras -> Sequence stroke -> [Sequence stroke]
reduceR n = iterate (rdropM n)

reduceR3 :: Matras -> Sequence stroke -> Sequence stroke -> Sequence stroke
reduceR3 dur sep = List.intercalate sep . take 3 . reduceR dur

-- | Start fully reduced, and expand to the given sequence.
expand :: Int -> Matras -> Sequence stroke -> [Sequence stroke]
expand times dur = reverse . take times . iterate (dropM dur)

replaceEnd :: Sequence stroke -> Sequence stroke -> Sequence stroke
replaceEnd seq suffix = rdropD (Solkattu.duration_of suffix) seq <> suffix

replaceStart :: Sequence stroke -> Sequence stroke -> Sequence stroke
replaceStart prefix seq = prefix <> dropD (Solkattu.duration_of prefix) seq

-- | Increase speed by a multiple by incrementing Speeds.
fasterS :: Solkattu.Speed -> Sequence stroke -> Sequence stroke
fasterS speed seq = seq

faster :: Sequence stroke -> Sequence stroke
faster = fasterS Solkattu.S2

-- | Decrease speed by a multiple by adding __ at S1 and decrementing higher
-- speeds.
slowerS :: Solkattu.Speed -> Sequence stroke -> Sequence stroke
slowerS speed = id
-- slowerS speed = concat . mapAccumL expand S1
--     where
--     expact speed n = case n of
--         TimeChange (Speed speed) -> case speed of
--             S1 ->
--             _ -> 

slower :: Sequence stroke -> Sequence stroke
slower = slowerS Solkattu.S2
