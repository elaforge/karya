-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Edo_test where
import qualified Data.Text as Text

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.NN as NN
import qualified Ui.UiTest as UiTest

import           Util.Test


test_edo :: Test
test_edo = do
    let run scaleId divisions intervals pitches = DeriveTest.extract extract $
            DeriveTest.deriveTracks (makeTitle scaleId intervals divisions) $
            UiTest.noteTrack1 pitches
        makeTitle scaleId intervals divisions =
            Text.intercalate " | " $ filter (/="")
                [ maybe "" ("edo-divisions = "<>) divisions
                , maybe "" ("edo-intervals = "<>) intervals
                , "scale=" <> scaleId
                ]
        extract = Score.initialNn
    stringsLike (snd $ run "edo" Nothing Nothing ["4a"])
        ["\"edo-divisions\": not found"]
    stringsLike (snd $ run "edo" (Just "12") (Just "(list 1 1)") ["4a"])
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
