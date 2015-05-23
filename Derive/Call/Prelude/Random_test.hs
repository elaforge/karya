-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TupleSections #-}
module Derive.Call.Prelude.Random_test where
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Seq as Seq
import Util.Test
import qualified Derive.Call.Prelude.Random as Random
import qualified Derive.DeriveTest as DeriveTest


test_omit = do
    let extract = DeriveTest.extract DeriveTest.e_start_dur
    let run n = extract $ DeriveTest.derive_tracks ""
            [(">", [(p, 1, n) | p <- Seq.range 0 5 1])]
    equal (run "omit 0 |") ([(p, 1) | p <- Seq.range 0 5 1], [])
    equal (run "omit 1 |") ([], [])
    let present = [0, 1, 2, 3]
    equal (run "omit .5 |") (map (, 1) present, [])

    -- Ensure different calls to the same block are differently random.
    let blocks ns = extract $ DeriveTest.derive_blocks
            [ ("top", [(">", [(p, 1, n) | (p, n) <- zip (Seq.range_ 0 1) ns])])
            , ("sub=ruler", [(">", [(0, 1, "omit .5 |")])])
            ]
    let present = [2, 4, 5, 6, 7, 9]
    equal (blocks (replicate 10 "sub")) ([(n, 1) | n <- present], [])

test_alternate = do
    let run s = DeriveTest.extract DeriveTest.e_pitch $ DeriveTest.derive_blocks
            [ ("top", [(">", [(p, 1, s) | p <- Seq.range 0 5 1])])
            , ("s1=ruler", [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")])])
            , ("s2=ruler", [(">", [(0, 1, "")]), ("*", [(0, 0, "4d")])])
            ]
    equal (run "alt s1 s2") (["4c", "4c", "4c", "4c", "4d", "4c"], [])

test_alternate_weighted = do
    let run s = DeriveTest.extract (DeriveTest.e_control "c") $
            DeriveTest.derive_tracks ""
                [(">", [(0, 1, "")]), ("c", [(0, 0, s)])]
    strings_like (snd (run "alt-w a b")) ["expected Num"]
    equal (run "alt-w 1 '5'") ([[(0, 5)]], [])
    equal (run "alt-w 1 5") ([[(0, 5)]], [])

    let runp s = DeriveTest.extract DeriveTest.e_note $
            DeriveTest.derive_tracks ""
                [(">", [(0, 1, "")]), ("*", [(0, 0, s)])]
    equal (runp "alt-w 1 '4c'") ([(0, 1, "4c")], [])
    strings_like (snd (runp "alt-w 1 (4c)")) ["pitches must be quoted"]

test_alternate_tracks = do
    let run tracks = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup (DeriveTest.with_skel skel) ""
                (map ((,) ">") tracks)
            where skel = map ((,) 1) [2 .. length tracks]
    equal (run [[(0, 1, "+a")]]) (["+a"], [])
    equal (run [[(0, 1, "alt-t 999")], [(0, 1, "+a")], [(0, 1, "+b")]])
        (["+a"], [])
    equal (run [[(0, 1, "alt-t 1 999")], [(0, 1, "+a")], [(0, 1, "+b")]])
        (["+b"], [])

test_pick_weighted = do
    let f weights = Random.pick_weighted
            (NonEmpty.fromList (zip ("abcdef" :: [Char]) weights))
    equal (map (f [1, 3]) [0, 0.25, 0.5, 0.75]) "abbb"
    equal (map (f [3, 1]) [0, 0.25, 0.5, 0.75]) "aaab"
