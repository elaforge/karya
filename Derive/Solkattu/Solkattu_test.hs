-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
module Derive.Solkattu.Solkattu_test where
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Solkattu.Dsl as Dsl
import Derive.Solkattu.Dsl (__)
import Derive.Solkattu.DslSollu (ta, di, ki, tha, thom)
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


test_verify_alignment = do
    let f = verify_alignment Tala.adi_tala
        tdkt = cycle $ ta <> di <> ki <> ta
    equal (f []) Nothing
    equal (f (ta <> ta)) (Just (2,
        "korvai should end on or before sam: avartanam 1, akshara 0, matra 2"))
    equal (f (take 6 tdkt)) (Just (6,
        "korvai should end on or before sam: avartanam 1, akshara 1, matra 2"))
    equal (f (take (8*4) tdkt)) Nothing
    equal (f (Dsl.speed (-2) $ take 8 tdkt)) Nothing
    -- Ok to end on sam, even with trailing rests.
    equal (f (Dsl.speed (-2) $ take 9 tdkt <> __ <> __)) Nothing
    -- But I don't drop rests because sometimes they make it line up.
    equal (f (Dsl.speed (-2) $ take 7 tdkt <> __)) Nothing

    equal (f (Dsl.speed (-2) $ take 4 tdkt <> Dsl.akshara 4 <> take 4 tdkt))
        Nothing
    equal (f (take 3 tdkt <> Dsl.akshara 4 <> take 5 tdkt))
        (Just (3, "expected akshara 4, but at avartanam 1, akshara 0, matra 3"))

test_verify_alignment_nadai_change = do
    let f = verify_alignment Tala.adi_tala
        tdkt = cycle $ ta <> di <> ki <> ta
    -- Change nadai in the middle of an akshara.
    equal (f (take 2 tdkt <> Dsl.nadai 6 (take 3 tdkt)))
        (Just (5, "korvai should end on or before sam:\
            \ avartanam 1, akshara 1, matra 0"))

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
    let sequence p7 = Dsl.nadai 8 (__ <> Dsl.repeat 5 p7)
            <> Dsl.nadai 6 (Dsl.tri p7)
    equal (f (sequence (ta <> __ <> di <> __ <> ki <> tha <> thom))) Nothing
    equal (f (sequence Dsl.p7)) Nothing

test_cancel_karvai = do
    let f = Text.unwords . map (pretty . snd) . Solkattu.cancel_karvai
            . Sequence.flatten
    equal (f (ta <> thom)) "ta thom"
    equal (f (ta <> Dsl.karvai thom)) "ta"
    equal (f (ta <> Dsl.karvai thom <> __)) "ta thom"
    equal (f (ta <> Dsl.karvai thom <> di)) "ta di"

verify_alignment :: Tala.Tala -> Korvai.Sequence -> Maybe (Int, Text)
verify_alignment tala = Solkattu.verify_alignment tala
    . map (first Sequence._tempo) . Sequence.flatten

test_vary = do
    let f notes = map (Text.unwords . map pretty) $
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
