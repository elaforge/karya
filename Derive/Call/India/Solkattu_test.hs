-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Solkattu_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Call.India.Solkattu as Solkattu
import Derive.Call.India.Solkattu
       (Sollu(..), Alignment(..), RealizedNote(..), Stroke(..), Valantalai(..))
import qualified Derive.Call.India.SolkattuDsl as SolkattuDsl
import Derive.Call.India.SolkattuDsl (ta, di, ki, kar)

import Global


test_realize_tala = do
    let f = (Text.intercalate "; " *** map pretty)
            . Solkattu.realize_tala (Solkattu.adi_tala 2)
    left_like (f (ta <> di <> ki <> ta)) "no karvai but there's unfilled space"
    equal (f (kar ta <> di <> ki <> ta))
        (Right ["ta", "__4", "di", "ki", "ta"])
    equal (f (kar ta <> di <> kar ki <> ta))
        (Right ["ta", "__2", "di", "ki", "__2", "ta"])
    left_like (f (kar ta <> kar di <> kar ki <> ta)) "uneven division"

realize_tala :: Solkattu.Korvai -> Either [Text] [Solkattu.RealizedNote]
realize_tala korvai =
    Solkattu.realize_tala (Solkattu.korvai_tala korvai)
        (Solkattu.korvai_sequence korvai)

test_verify_durations = do
    let f = map (fmap (second pretty))
            . Solkattu.verify_durations (Solkattu.adi_tala 2)
        tdkt = cycle $ ta <> di <> ki <> ta
    -- sam->arudi, arudi->sam
    equal (f [(Sam, take 8 tdkt), (Arudi, take 8 tdkt)])
        [ Right (0, "[ta, di, ki, ta, ta, di, ki, ta]")
        , Right (0, "[ta, di, ki, ta, ta, di, ki, ta]")
        ]
    equal (f [(Sam, take 6 tdkt), (Arudi, take 6 tdkt)])
        [ Right (2, "[ta, di, ki, ta, ta, di]")
        , Right (2, "[ta, di, ki, ta, ta, di]")
        ]
    equal (f [(Sam, take 9 tdkt), (Arudi, take 10 tdkt)])
        [ Left "Sam->Arudi transition should have <= 8 matras, but has 9"
        , Left "Arudi->Sam transition should have <= 8 matras, but has 10"
        ]
    equal (f [(Sam, take 4 tdkt)]) [Right (12, "[ta, di, ki, ta]")]
    equal (f [(Sam, take 8 tdkt)])
        [Right (8, "[ta, di, ki, ta, ta, di, ki, ta]")]
    equal (f [(Sam, take 9 tdkt)])
        [Right (7, "[ta, di, ki, ta, ta, di, ki, ta, ta]")]

test_realize_karvai = do
    let f dur = second (Text.unwords . map pretty) . Solkattu.realize_karvai dur
    left_like (f 2 (ta <> di)) "no karvai but there's unfilled space"
    equal (f 2 (ta <> kar di)) (Right "ta di __ __")

test_realize_mridangam = do
    let f = (Text.unlines *** show_strokes)
            . Solkattu.realize_mridangam SolkattuDsl.simple_patterns mmap
        mmap = Map.fromList
            [ ([Ta, Din], [k, od])
            , ([Ta], [t])
            ]
        k = Solkattu.Valantalai Solkattu.MKi
        t = Solkattu.Valantalai Solkattu.MTa
        od = Both Solkattu.MThom Solkattu.MDin
    equal (f [RRest 1, RSollu Ta Nothing, RRest 2, RSollu Din Nothing])
        (Right "- k - - od")
    equal (f [RPattern 5, RRest 1, RSollu Ta Nothing, RSollu Din Nothing])
        (Right "k t k n o - k od")
    equal (f [RSollu Ta Nothing, RSollu Ta Nothing]) (Right "t t")
    left_like (f [RSollu Din Nothing, RSollu Din Nothing]) "sequence not found"

show_strokes :: [Solkattu.MNote] -> Text
show_strokes = Text.unwords . map pretty

test_check_mridangam_map = do
    let f = fmap Map.toList . Solkattu.check_mridangam_map
        (k, t) = (SolkattuDsl.k, SolkattuDsl.t)
    equal (f []) (Right [])
    equal (f [(ta <> di, [k, t])])
        (Right [([Ta, Di], [Valantalai MKi, Valantalai MTa])])
    left_like (f (replicate 2 (ta <> di, [k, t]))) "duplicate mridangam keys"
    left_like (f [(ta <> di, [k])]) "have differing lengths"
    left_like (f [(ta <> SolkattuDsl.din_, [k])]) "only have plain sollus"

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
