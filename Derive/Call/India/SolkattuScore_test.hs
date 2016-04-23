-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.SolkattuScore_test where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Derive.Call.India.Solkattu as Solkattu
import qualified Derive.Call.India.SolkattuDsl as SolkattuDsl
import qualified Derive.Call.India.SolkattuScore as SolkattuScore

import Global


show_realize_korvai = do
    let k1 = SolkattuScore.kandam1 (SolkattuDsl.pat 6)
            (SolkattuDsl.thom <> SolkattuDsl.__)
    put (realize 20 k1)

    put (realize 16 SolkattuScore.chatusram1_2)
    put (realize 16 SolkattuScore.chatusram1_3)
    put (realize 16 SolkattuScore.chatusram1_4)

put :: Text -> IO ()
put = Text.IO.putStrLn

realize :: Int -> Solkattu.Korvai -> Text
realize chunk_size = Solkattu.show_strokes chunk_size
    . Solkattu.check . first Text.unlines
    . Solkattu.realize_korvai SolkattuDsl.simple_patterns
