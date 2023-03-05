-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Raga_test where
import qualified Util.Seq as Seq
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale.Raga as Raga
import qualified Derive.Score as Score

import qualified Perform.NN as NN

import           Util.Test


test_pitch :: Test
test_pitch = do
    let run key base ps = DeriveTest.extract extract $
            DeriveTest.derive_tracks ""
            [ (title, [(t, 1, "") | (t, _) <- times ps])
            , ("*raga", [(t, 0, p) | (t, p) <- times ps])
            ]
            where
            times = zip (Seq.range_ 0 1)
            title = "> | just-base = " <> base <> " | key = " <> key
        extract = Score.initial_nn
    equalf 0.01 (run "hemavati" "(hz g3)" ["3s"]) ([Just NN.g3], [])

test_alternate :: Test
test_alternate = do
    let f = Raga.alternate [("a", "BC"), ("c", "DE")]
    equal (f []) []
    equal (f "xz") ["xz"]
    equal (f "xyaz") ["xyaz", "xyBCz"]
    equal (f "xacz") ["xacz", "xaDEz", "xBCcz", "xBCDEz"]
    equal (Raga.alternate Raga.alternates "kamboji")
        ["kamboji","kambhoji","khamboji","khambhoji"]
