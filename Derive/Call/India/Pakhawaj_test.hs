-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Pakhawaj_test where
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Call.India.Pakhawaj as Pakhawaj
import Derive.Call.India.Pakhawaj (Bol(..), Stroke(..), Sequence(..), Speed(..))


test_infer_tette = do
    let f = Foldable.toList . Pakhawaj.infer_tette . Speed S1 . map Note
    equal (f [One Tette]) [One Tet]
    equal (f (map One [Tette, Tette])) (map One [Tet, Te])
    equal (f (map One [Tette, Tette, Tette])) (map One [Tet, Te, Tet])
    equal (f [Together Ka Tette, One Te]) [Together Ka Tet, One Te]
    equal (f [Together Ka Tette, One Tet]) [Together Ka Te, One Tet]

test_match_syllables = do
    let f = Pakhawaj.match_syllables . Text.words
    equal (f "dha") (Right $ Speed S1 [Note $ Together Ge Ta])
    equal (f "te re ki ta ta")
        (Right $ Speed S1 $ map (Note . One) [Tet, Te, Ka, Tet, Ta])

test_simplify = do
    let f = Pakhawaj.simplify
    equal (f $ Speed S1 [Speed S1 [Note 'a'], Note 'b'])
        (Speed S1 [Note 'a', Note 'b'])
    equal (f $ Speed S1 [Speed S2 [Note 'a'], Note 'b'])
        (Speed S1 [Speed S2 [Note 'a'], Note 'b'])
    equal (f $ Speed S1 [Speed S2 [Speed S2 [Note 'a']]])
        (Speed S1 [Speed S2 [Note 'a']])
