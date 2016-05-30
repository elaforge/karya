-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Solkattu.Score_test where
import Util.Test
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Dsl as Dsl
import qualified Derive.Solkattu.Score as Score

import Global


test_kandam = do
    let korvais = Score.k1_1_vars ++ [Score.k1_2, Score.k1_3]
    equal [err | Left err <- map realize korvais] []

realize :: Solkattu.Korvai -> Either Text [Solkattu.MNote]
realize korvai =
    Solkattu.realize_korvai Dsl.default_patterns korvai
