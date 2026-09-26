-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.India.Pakhawaj_test where
import qualified Data.Text as Text

import Util.Test
import qualified Derive.C.India.Pakhawaj as Pakhawaj
import Derive.C.India.Pakhawaj (Bol(..), Stroke(..), Note(..))
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_c_bols :: Test
test_c_bols = do
    let run dur bols = DeriveTest.extract extract $
            DeriveTest.deriveTracksLinear "import india.pakhawaj"
                [(">", [(0, dur, "bols")]), (">", bols)]
        extract e = (Score.eventStart e, DeriveTest.eAttributes e)
    equal (run 8 [(0, 0, "tet"), (1, 0, "te")]) ([(0, "+tet"), (1, "+te")], [])
    equal (run 8 [(0, 0, "tetekata")])
        ([(0, "+tet"), (1, "+te"), (2, "+ka"), (3, "+ta"), (4, "+ge"),
            (5, "+di"), (6, "+ge"), (7, "+ne")], [])

test_realize_bols :: Test
test_realize_bols = do
    let f = Pakhawaj.realizeBols 1.5
    leftLike (f [(0, "dha"), (1, "blah")]) "unknown bol"
    equal (f [(0, "dha"), (1, "ta")]) $
        Right [(0, Together Ge Ta), (1, One Ta)]
    equal (f [(0, "kt"), (1, "tk")]) $
        Right [(0, One Tet), (0.5, One Te), (1, One Ka), (1.25, One Tet)]

test_infer_tette :: Test
test_infer_tette = do
    let f = Pakhawaj.inferTette
    equal (f [One Tette]) [One Tet]
    equal (f (map One [Tette, Tette])) (map One [Tet, Te])
    equal (f (map One [Tette, Tette, Tette])) (map One [Tet, Te, Tet])
    equal (f [Together Ka Tette, One Te]) [Together Ka Tet, One Te]
    equal (f [Together Ka Tette, One Tet]) [Together Ka Te, One Tet]

test_match_syllables :: Test
test_match_syllables = do
    let f = fmap (map snd) . Pakhawaj.matchSyllables . map ((,) ())
            . Text.words
    equal (f "dha") (Right [Note $ Together Ge Ta])
    equal (f "te re ki ta ta")
        (Right $ map (Note . One) [Tet, Te, Ka, Tet, Ta])
