-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
module Derive.Solkattu.Solkattu_test where
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Solkattu.Dsl as Dsl
import Derive.Solkattu.Dsl (ta, di, ki, tha, thom, __)
import qualified Derive.Solkattu.Solkattu as Solkattu

import Global


test_verify_alignment = do
    let f = verify_alignment
        tdkt = cycle $ ta <> di <> ki <> ta
        tala4 = Solkattu.Tala 4 2 2 -- 4 akshara, 2 nadai
    equal (f tala4 []) []
    strings_like (f tala4 ta) ["ta", "akshara 0, matra 1"]
    strings_like (f tala4 (take 4 tdkt)) ["ta di ki ta", "akshara 2, matra 0"]
    equal (f tala4 (take 8 tdkt)) []
    equal (f tala4 (take 4 tdkt <> Dsl.atX <> take 4 tdkt)) []
    strings_like (f tala4 (take 3 tdkt <> Dsl.atX <> take 5 tdkt))
        [ "ta di ki"
        , "expected akshara 2, but at avartanam 1, akshara 1, matra 1"
        ]

test_verify_alignment_nadai_change = do
    let f = verify_alignment
        tdkt = cycle $ ta <> di <> ki <> ta
    -- Change nadai in the middle of an akshara.
    let tala8 = Solkattu.Tala 1 0 8
    equal (f tala8 (take 4 tdkt <> Dsl.nadai 6 <> take 3 tdkt))
        []
    strings_like (f tala8 (take 4 tdkt <> Dsl.nadai 6 <> take 4 tdkt))
        ["ta di ki", "avartanam 2, akshara 0, matra 1"]
    strings_like (f tala8 (take 3 tdkt <> Dsl.nadai 6 <> take 4 tdkt))
        ["ta di ki", "can't change nadai 8->6"]

    -- More complicated example:
    -- 0 __ Ta __ di __ ki th tm
    -- 1 Ta __ di __ Ki th tm Ta
    -- 2 __ di __ ki Th tm Ta __
    -- 3 di __ ki th Tm Ta __ di
    -- 4 __ ki th tm Ta __ di
    --               nadai 6
    -- 5 -_ ki th Tm ta __
    -- 6 di __ ki th tm ta
    -- 7 __ di __ ki th tm
    let sequence p7 = __ <> Dsl.repeat 5 p7 <> Dsl.nadai 6 <> Dsl.tri p7
    let adi = Solkattu.adi_tala 8
    equal (f adi (sequence (ta <> __ <> di <> __ <> ki <> tha <> thom))) []
    equal (f adi (sequence Dsl.p7)) []

verify_alignment :: Solkattu.Tala -> [Solkattu.Note ()] -> [Text]
verify_alignment tala = snd . Solkattu.verify_alignment tala

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
