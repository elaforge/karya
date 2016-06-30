-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
module Derive.Solkattu.Solkattu_test where
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Solkattu.Dsl as Dsl
import Derive.Solkattu.Dsl (ta, di, ki)
import qualified Derive.Solkattu.Solkattu as Solkattu

import Global


test_verify_alignment = do
    let f (notes :: [Solkattu.Note ()]) = first (Text.intercalate "; ") $
            Solkattu.verify_alignment (Solkattu.Tala 4 2 2) notes
        tdkt = cycle $ ta <> di <> ki <> ta
    equal (f []) (Right [])
    left_like (f ta) "expected Akshara 0"
    left_like (f (take 4 tdkt)) "expected Akshara 0"
    equal (f (take 8 tdkt)) (Right (take 8 tdkt))
    equal (f (take 4 tdkt <> Dsl.atX <> take 4 tdkt)) (Right (take 8 tdkt))
    left_like (f (take 3 tdkt <> Dsl.atX <> take 4 tdkt)) "expected Arudi"

test_vary = do
    let f (notes :: [Solkattu.Note ()]) = map (Text.unwords . map pretty) $
            Solkattu.vary
                (Solkattu.variations [Solkattu.standard, Solkattu.ascending])
                notes
    equal (f (ta <> di)) ["ta di"]
    equal (f (ta <> Dsl.p6 <> di <> Dsl.p6)) ["ta p6 di p6"]
    equal (f (ta <> Dsl.p6 <> di <> Dsl.p6 <> Dsl.p6)) ["ta p5 di p6 p7"]
    equal (f (Dsl.tri_ ta Dsl.p6 <> di <> Dsl.tri_ ki Dsl.p7))
        [ "p5 ta p6 ta p7 di p6 ki p7 ki p8"
        , "p5 ta p6 ta p7 di p5 ki p7 ki p9"
        ]

-- * utils

test_split_just = do
    let f = Solkattu.split_just
    equal (f (flip lookup [(2, 'b')]) 'a' [1, 2, 3]) [('a', [1]), ('b', [2, 3])]

test_group_rights = do
    let f = Solkattu.group_rights
    equal (f [Left 'a', Right 'b', Right 'c', Left 'd'])
        [Left 'a', Right "bc", Left 'd']

test_round_up = do
    let f = Solkattu.round_up
    equal (f 7 8) 8
    equal (f 8 8) 8
    equal (f 9 8) 16

test_apply_modifications = do
    let f = Solkattu.apply_modifications (+)
    equal (f [] [1]) [1]
    let mods = [(0, 10), (2, 20)]
    equal (f mods [1]) [11]
    equal (f mods [1..2]) [11, 2]
    equal (f mods [1..3]) [11, 2, 23]
    equal (f mods [1..4]) [11, 2, 23, 4]

test_permute_fst = do
    let f = Solkattu.permute_fst (\x -> [x, x+1])
    equal (f ([] :: [(Int, Char)])) []
    equal (f [(0, 'a')]) [[(0, 'a')], [(1, 'a')]]
    equal (f [(0, 'a'), (10, 'b')])
        [ [(0, 'a'), (10, 'b')]
        , [(0, 'a'), (11, 'b')]
        , [(1, 'a'), (10, 'b')]
        , [(1, 'a'), (11, 'b')]
        ]
