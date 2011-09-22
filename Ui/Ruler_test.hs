module Ui.Ruler_test where
import qualified Data.Map as Map

import Util.Control
import Util.Test
import Ui
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime


marklist :: [ScoreTime] -> Ruler.Marklist
marklist ps = Ruler.Marklist $
    Map.fromList [(p, mark (floor (ScoreTime.to_double p))) | p <- ps]

extract :: Ruler.Marklist -> [(ScoreTime, Int)]
extract (Ruler.Marklist a) =
    map (\(p, m) -> (p, Ruler.mark_rank m)) (Map.toList a)

mark :: Int -> Ruler.Mark
mark rank = Ruler.null_mark { Ruler.mark_rank = rank }

test_concat = do
    equal (extract (marklist [0, 1] <> marklist [1, 2]))
        [(0, 0), (1, 1), (2, 2)]
    equal (extract (marklist [0, 1] <> marklist [4]))
        [(0, 0), (1, 1), (4, 4)]
    equal (extract (marklist [0, 1] <> marklist []))
        [(0, 0), (1, 1)]

test_place = do
    let f s d = extract . Ruler.place s d
    equal (f 2 4 (marklist [0, 1, 2])) [(2, 0), (4, 1), (6, 2)]
    equal (f 2 0 (marklist [0, 1, 2])) [(2, 2)]

test_place_marklists = do
    let f = extract . Ruler.place_marklists
    equal (extract (Ruler.place 0 4 (marklist [0, 1]))) [(0, 0), (4, 1)]
    equal (f [(0, 4, marklist [0, 1])]) [(0, 0), (4, 1)]
    equal (f [(0, 4, marklist [0, 1]), (4, 2, marklist [0, 1])])
        [(0, 0), (4, 0), (6, 1)]
