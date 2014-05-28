-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Idiom.String_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_string = do
    let extract = DeriveTest.extract e_event
        -- 'merge_curve' can't see that pitches are the same, so it can produce
        -- duplicate pitches.
        e_event e = (Score.event_start e,
            Seq.drop_dups snd $ DeriveTest.e_nns e)
    let run p1 p2 p3 = extract $ DeriveTest.derive_tracks
            "import idiom.string | open-strings = '4c 4d 4e 4g 4a'"
            [ ("> | bent-string 2 2 1", [(0, 5, ""), (5, 5, ""), (10, 5, "")])
            , ("*", [(0, 0, p1), (5, 0, p2), (10, 0, p3)])
            ]
    let (res, logs) = run "4c" "2d" "2e"
    equal res [(0, [(0, 60)])]
    strings_like logs ["38nn below lowest string", "40nn below lowest string"]

    -- All separate strings doesn't do anything interesting.
    equal (run "4c" "4d" "4e")
        ([ (0, [(0, 60)])
        , (5, [(5, 62)])
        , (10, [(10, 64)])
        ], [])

    equal (run "4c" "4c#" "4d")
        ([ (0, [(0, 60), (4, 60.5), (5, 61)]) -- bend up for attack
        , (5, [(5, 61), (12, 60.5), (13, 60)]) -- bend back down for release
        , (10, [(10, 62)])
        ], [])

    strings_like (snd $ extract $ DeriveTest.derive_tracks
            "import idiom.string | bent-string" [(">", [(0, 1, "")])])
        ["open-strings required"]
