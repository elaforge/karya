-- | Utilities for Carnatic music.
module Cmd.Repl.LTala where
import Util.Control
import qualified Ui.ScoreTime as ScoreTime
import Types

-- 4x 3x 2x 1x + x + 4x 3x 2x 1x + x + 4x 3x 2x 1x = 32x
m3 = pattern1 ["tha", "ki", "ta"]
m3m = pattern1 ["dit", "thom", "thom", "ka"]
m4 = pattern1 ["ta", "ka", "din", "na"]
m4m = pattern1 ["ki", "ta", "ki", "nam", "thom"]
m5 = pattern1 ["ta", "ti", "ki", "ta", "thom"]

pattern1 = pattern1_ 0.25

pattern1_ :: ScoreTime -> [Text] -> [(ScoreTime, ScoreTime, Text)]
pattern1_ factor sol =
    map multiply $ expand $ once ++ gap ++ once ++ gap ++ once
    where
    once = concatMap (\n -> map ((,) n) sol) [4, 3, 2, 1]
    gap = [(ScoreTime.double (fromIntegral (length sol)), "thom")]
    expand notes = zip3 (scanl (+) 0 durs) (repeat 0) ns
        where (durs, ns) = unzip notes
    multiply (start, dur, text) = (start * factor, dur * factor, text)
