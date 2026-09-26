-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Wayang_test where
import qualified Data.Vector as Vector

import qualified Util.Lists as Lists
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_read :: Test
test_read = do
    let f scaleId pitch = readScale (getScale scaleId) pitch
    -- The same pitch also winds up with the same Pitch and same frequency.
    equal (f "wayang" "5i") (Right "5-0")
    equal (f "wayang-pemade" "i^") (Right "5-0")
    equal (f "wayang-kantilan" "i-") (Right "5-0")
    let run scale pitch = DeriveTest.extract Score.initialNn $
            DeriveTest.deriveTracks "" $ scaleTrack scale [pitch]
    equal (run "wayang" "5i") (run "wayang-pemade" "i^")
    equal (run "wayang" "6i") (run "wayang-kantilan" "i^")

test_note_to_call :: Test
test_note_to_call = do
    let run laras = ScaleTest.noteToCall "wayang" ("laras=" <> laras)
        umbang = Vector.toList $ BaliScales.larasUmbang Wayang.larasSawan
    equal (run "sawan" ["1i"]) ([Just (head umbang)], [])
    equal (run "sawan" ["4i"]) ([Just (umbang !! (3*5))], [])
    let toHz = first $ map $ fmap (round . Pitch.nnToHz)
    equal (toHz $ run "gender-kuta" ["3o", "3e"]) ([Just 183, Just 206], [])

test_input_to_note :: Test
test_input_to_note = do
    let f scale = ScaleTest.inputToNote scale mempty
        wayang = getScale "wayang"
        wayangPemade = getScale "wayang-pemade"
        invalid = "invalid input"
    equal (f wayang (1, 0, 0)) "1i"
    equal (f wayangPemade (3, 1, 0)) "o_"
    equal (f wayangPemade (4, 0, 0)) "i-"
    equal (map (f wayang) [(4, pc, 0) | pc <- [0..5]])
        ["4i", "4o", "4e", "4u", "4a", "5i"]
    equal (map (f wayangPemade) $
            take 12 [(oct, pc, 0) | oct <- [3..6], pc <- [0..4]])
        [ invalid, "o_", "e_", "u_", "a_", "i-", "o-", "e-", "u-", "a-"
        , "i^", invalid
        ]

test_input_to_nn :: Test
test_input_to_nn = do
    let pemade = getScale "wayang-pemade"
        kantilan = getScale "wayang-kantilan"
    let run scale = DeriveTest.eval Ui.empty . Scale.scaleInputToNn scale 0
            . ScaleTest.asciiKbd
    equal (run pemade (4, 0, 0)) (Right (Right 62.5))
    equal (run kantilan (4, 0, 0)) (Right (Left DeriveT.InvalidInput))
    equal (run pemade (5, 0, 0)) (Right (Right 74.66))
    equal (run kantilan (5, 0, 0)) (Right (Right 74.57))

getScale :: Text -> Scale.Scale
getScale = ScaleTest.getScale Wayang.scales

-- * util

readScale :: Scale.Scale -> Pitch.Note -> Either Text Text
readScale scale note = bimap pretty pretty $
    Scale.scaleRead scale mempty note

scaleTrack :: Text -> [Text] -> [UiTest.TrackSpec]
scaleTrack scaleId pitches =
    [ (">", [(n, 1, "") | n <- map fst events])
    , ("*" <> scaleId, [(n, 0, p) | (n, p) <- events])
    ]
    where events = zip (Lists.range_ 0 1) pitches
