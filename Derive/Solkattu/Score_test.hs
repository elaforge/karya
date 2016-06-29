-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Solkattu.Score_test where
import Util.Test
import qualified Derive.Solkattu.Score as Score
import qualified Derive.Solkattu.Solkattu as Solkattu
import Global


test_tisrams = do
    equal [err | Left err <- map realize Score.tisrams] []

test_chatusrams = do
    equal [err | Left err <- map realize Score.chatusrams] []

test_kandams = do
    equal [err | Left err <- map realize Score.kandams] []

realize :: Solkattu.Korvai -> Either Text [Solkattu.MNote]
realize = Solkattu.realize_korvai
