-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Edo_test where
import qualified Data.Text as Text

import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.NN as NN

import Global


test_edo = do
    let run scale_id divisions intervals pitches = DeriveTest.extract extract $
            DeriveTest.derive_tracks (make_title scale_id intervals divisions) $
            UiTest.note_track1 pitches
        make_title scale_id intervals divisions =
            Text.intercalate " | " $ filter (/="")
                [ maybe "" ("edo-divisions = "<>) divisions
                , maybe "" ("edo-intervals = "<>) intervals
                , "scale=" <> scale_id
                ]
        extract = Score.initial_nn
    strings_like (snd $ run "edo" Nothing Nothing ["4a"])
        ["\"edo-divisions\": not found"]
    strings_like (snd $ run "edo" (Just "12") (Just "(list 1 1)") ["4a"])
        ["should equal divisions"]

    equal (run "edo" (Just "12") Nothing ["4a", "4b", "4c"])
        (map Just [NN.c4, NN.cs4, NN.d4], [])
    equal (run "edo" (Just "12") (Just "(list 2 2 1 2 2 2 1)")
            ["4a", "4b", "4c"])
        (map Just [NN.c4, NN.d4, NN.e4], [])

    equalf 0.001 (run "edo" (Just "5") Nothing
            ["4a", "4b", "4c", "4d", "4e", "5a"])
        (map Just [60, 62.4, 64.8, 67.2, 69.6, 72], [])
    equalf 0.001 (run "edo" (Just "5") (Just "(list 2 1 2)")
            ["4a", "4a#", "4b", "4c", "4c#", "5a"])
        (map Just [60, 62.4, 64.8, 67.2, 69.6, 72], [])
