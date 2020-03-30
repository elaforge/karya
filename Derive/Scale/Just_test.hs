-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Just_test where
import qualified Data.Text as Text

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Ui.UiTest as UiTest

import           Util.Test


test_make_just7 = do
    let run scale_id intervals ratios pitches = DeriveTest.extract extract $
            DeriveTest.derive_tracks (make_title scale_id intervals ratios) $
            UiTest.note_track1 pitches
        make_title scale_id intervals ratios =
            Text.intercalate " | " $ filter (/="")
                [ maybe "" ("just-ratios = "<>) ratios
                , maybe "" ("just-intervals = "<>) intervals
                , "scale=" <> scale_id
                ]
        extract = fmap Pitch.nn_to_hz . Score.initial_nn
    strings_like (snd $ run "make-just7" Nothing Nothing ["4c"])
        ["\"just-ratios\": not found"]
    strings_like
        (snd $ run "make-just7" Nothing (Just "(list 9/8 5/4 4/3)") ["4c"])
        ["length should be sum"]

    let ratios = "(list 9/8 5/4 4/3 3/2 5/3 15/8)"
    let hz = Pitch.nn_to_hz NN.c4
        limit5 = map Just [hz, hz * (9/8), hz * (5/4)]
    equalf 0.001 (run "make-just7" Nothing (Just ratios) ["4c", "4d", "4e"])
        (limit5, [])
    equalf 0.001 (run "make-just7-r" Nothing (Just ratios) ["4s", "4r", "4g"])
        (limit5, [])
    equalf 0.001 (run "make-just7" (Just "maj") (Just "limit-5")
            ["4c", "4d", "4e"])
        (limit5, [])
