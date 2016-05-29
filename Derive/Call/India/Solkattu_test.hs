-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Solkattu_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Call.India.Solkattu as Solkattu
import Derive.Call.India.Solkattu
       (Note(..), Sollu(..), Stroke(..), Valantalai(..))
import qualified Derive.Call.India.SolkattuDsl as SolkattuDsl
import Derive.Call.India.SolkattuDsl (ta, di, ki, __)

import Global


test_verify_alignment = do
    let f = first (Text.intercalate "; ")
            . Solkattu.verify_alignment (Solkattu.Tala 4 2 2)
        tdkt = cycle $ ta <> di <> ki <> ta
    equal (f []) (Right [])
    left_like (f ta) "expected Sam"
    left_like (f (take 4 tdkt)) "expected Sam"
    equal (f (take 8 tdkt)) (Right (take 8 tdkt))
    equal (f (take 4 tdkt <> SolkattuDsl.atX <> take 4 tdkt))
        (Right (take 8 tdkt))
    left_like (f (take 3 tdkt <> SolkattuDsl.atX <> take 4 tdkt))
        "expected Arudi"

test_realize_mridangam = do
    let f = (Text.unlines *** show_strokes)
            . Solkattu.realize_mridangam SolkattuDsl.default_patterns smap
        smap = Solkattu.StrokeMap $ Map.fromList
            [ ([Ta, Din], [k, od])
            , ([Ta], [t])
            ]
        k = Solkattu.Valantalai Solkattu.MKi
        t = Solkattu.Valantalai Solkattu.MTa
        od = Both Solkattu.MThom Solkattu.MDin
    equal (f [Rest, Sollu Ta Nothing, Rest, Rest, Sollu Din Nothing])
        (Right "- k - - od")
    equal (f [Pattern 5, Rest, Sollu Ta Nothing, Sollu Din Nothing])
        (Right "k t k n o - k od")
    equal (f [Sollu Ta Nothing, Sollu Ta Nothing]) (Right "t t")
    left_like (f [Sollu Din Nothing, Sollu Din Nothing]) "sequence not found"

show_strokes :: [Solkattu.MNote] -> Text
show_strokes = Text.unwords . map pretty

test_stroke_map = do
    let f = fmap (\(Solkattu.StrokeMap smap) -> Map.toList smap)
            . Solkattu.stroke_map
        (k, t) = (SolkattuDsl.k, SolkattuDsl.t)
    equal (f []) (Right [])
    equal (f [(ta <> di, [k, t])])
        (Right [([Ta, Di], [Valantalai MKi, Valantalai MTa])])
    left_like (f (replicate 2 (ta <> di, [k, t]))) "duplicate mridangam keys"
    left_like (f [(ta <> di, [k])]) "have differing lengths"
    left_like (f [(ta <> __, [k])]) "only have plain sollus"

-- * utils

test_split_just = do
    let f = Solkattu.split_just
    equal (f (flip lookup [(2, 'b')]) 'a' [1, 2, 3])
        [('a', [1]), ('b', [2, 3])]

test_group_rights = do
    let f = Solkattu.group_rights
    equal (f [Left 'a', Right 'b', Right 'c', Left 'd'])
        [Left 'a', Right "bc", Left 'd']

test_round_up = do
    let f = Solkattu.round_up
    equal (f 7 8) 8
    equal (f 8 8) 8
    equal (f 9 8) 16
