-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Just_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import Global


test_make_just7 = do
    let run scale_id ratios pitch = DeriveTest.extract extract $
            DeriveTest.derive_tracks (make_title ratios)
                [(">", [(0, 1, "")]), ("*" <> scale_id, [(0, 0, pitch)])]
        make_title ratios = maybe "" ("just-ratios = "<>) ratios
        extract = fmap Pitch.nn_to_hz . Score.initial_nn
    strings_like (snd $ run "make-just7" Nothing "4c")
        ["\"just-ratios\": not found"]
    strings_like (snd $ run "make-just7" (Just "(list 9/8 5/4 4/3)") "4c")
        ["length should be exactly 6"]

    let ratios = "(list 9/8 5/4 4/3 3/2 5/3 15/8)"
    let hz = Pitch.nn_to_hz NN.c4
    equalf 0.001 (map (run "make-just7" (Just ratios)) ["4c", "4d", "4e"])
        [([Just hz], []), ([Just (hz * (9/8))], []), ([Just (hz * (5/4))], [])]
    equalf 0.001 (map (run "make-just7-r" (Just ratios)) ["4s", "4r", "4g"])
        [([Just hz], []), ([Just (hz * (9/8))], []), ([Just (hz * (5/4))], [])]
