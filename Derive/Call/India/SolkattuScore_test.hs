-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.SolkattuScore_test where
import Util.Test
import qualified Derive.Call.India.Solkattu as Solkattu
import qualified Derive.Call.India.SolkattuDsl as SolkattuDsl
import qualified Derive.Call.India.SolkattuScore as SolkattuScore

import Global


test_kandam = do
    equal [err | Left err <- map realize SolkattuScore.kandam1_vars] []

realize :: Solkattu.Korvai -> Either Text [Solkattu.MNote]
realize korvai =
    Solkattu.realize_korvai SolkattuDsl.default_patterns
        SolkattuDsl.default_karvai korvai
