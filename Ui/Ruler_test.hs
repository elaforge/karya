module Ui.Ruler_test where
import qualified Data.Map as Map

import Util.Control
import Util.Test
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import Types


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
