-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Pakhawaj_test where
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Call.India.Pakhawaj as Pakhawaj
import Derive.Call.India.Pakhawaj (Bol(..), Stroke(..), Note(..))
import Global


test_realize_bols = do
    let f = first untxt . Pakhawaj.realize_bols 0.5
    left_like (f [(0, "dha"), (1, "blah")]) "unknown bol"
    equal (f [(0, "dha"), (1, "ta")]) $
        Right [(0, Together Ge Ta), (1, One Ta)]
    equal (f [(0, "kt"), (1, "tk")]) $
        Right [(0, One Tet), (0.5, One Te), (1, One Ka), (1.25, One Tet)]

test_infer_tette = do
    let f = Pakhawaj.infer_tette
    equal (f [One Tette]) [One Tet]
    equal (f (map One [Tette, Tette])) (map One [Tet, Te])
    equal (f (map One [Tette, Tette, Tette])) (map One [Tet, Te, Tet])
    equal (f [Together Ka Tette, One Te]) [Together Ka Tet, One Te]
    equal (f [Together Ka Tette, One Tet]) [Together Ka Te, One Tet]

test_match_syllables = do
    let f = fmap (map snd) . Pakhawaj.match_syllables . map ((,) ())
            . Text.words
    equal (f "dha") (Right [Note $ Together Ge Ta])
    equal (f "te re ki ta ta")
        (Right $ map (Note . One) [Tet, Te, Ka, Tet, Ta])
