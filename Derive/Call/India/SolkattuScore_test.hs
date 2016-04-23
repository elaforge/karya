-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.SolkattuScore_test where
import Util.Test
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Derive.Call.India.Solkattu as Solkattu
import qualified Derive.Call.India.SolkattuDsl as SolkattuDsl
import qualified Derive.Call.India.SolkattuScore as SolkattuScore

import Global


test_kandam = do
    equal [err | Left err <- map realize SolkattuScore.kandam1_var] []

show_realize_korvai = do
    let k1 = SolkattuScore.kandam1 (SolkattuDsl.pat 6)
            (SolkattuDsl.thom <> SolkattuDsl.__)
    put (Solkattu.check $ realize k1)
    put (Solkattu.check $ realize SolkattuScore.chatusram1_2)
    put (Solkattu.check $ realize SolkattuScore.chatusram1_3)
    put (Solkattu.check $ realize SolkattuScore.chatusram1_4)

put :: Text -> IO ()
put = Text.IO.putStrLn

realize :: Solkattu.Korvai -> Either Text Text
realize korvai =
    (Text.unlines *** Solkattu.show_strokes size) $
    Solkattu.realize_korvai SolkattuDsl.simple_patterns korvai
    where
    tala = Solkattu.korvai_tala korvai
    size = (Solkattu.tala_aksharas tala `div` 2) * Solkattu.tala_nadai tala
