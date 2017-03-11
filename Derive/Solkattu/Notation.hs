-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Generic combinators for solkattu patterns.
module Derive.Solkattu.Notation where
import qualified Data.List as List

import qualified Util.CallStack as CallStack
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Solkattu (Matras, Sequence)
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
            _ -> dropM (matras - Solkattu.note_duration n) ns

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

rdropM :: Matras -> Sequence stroke -> Sequence stroke
rdropM matras = reverse . dropM matras . reverse

-- | Reduce three times, with a separator.
reduce3 :: Matras -> Sequence stroke -> Sequence stroke -> Sequence stroke
reduce3 n sep = List.intercalate sep . take 3 . iterate (dropM n)

-- | Reduce by a duration until a final duration.
reduceTo :: CallStack.Stack => Matras -> Matras -> Sequence stroke
    -> Sequence stroke
reduceTo by to seq
    | (Solkattu.duration_of seq - to) `mod` by /= 0 =
        errorStack $ showt (Solkattu.duration_of seq) <> " can't reduce by "
            <> showt by <> " to " <> showt to
    | otherwise = mconcat $ takeWhile ((>=to) . Solkattu.duration_of) $
        iterate (dropM by) seq

-- | Reduce by dropping the end.
reduceR :: Matras -> Sequence stroke -> [Sequence stroke]
reduceR n = iterate (rdropM n)

reduceR3 :: Matras -> Sequence stroke -> Sequence stroke -> Sequence stroke
reduceR3 dur sep = List.intercalate sep . take 3 . reduceR dur

-- | Start fully reduced, and expand to the given sequence.
expand :: Int -> Matras -> Sequence stroke -> [Sequence stroke]
expand times dur = reverse . take times . iterate (dropM dur)
