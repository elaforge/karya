module Derive.Note_test where
import Util.Test

import qualified Derive.DeriveTest as DeriveTest


-- * derivers

test_c_note = do
    -- Test basic Derive.d_note_track plumbing and the note (null) deriver
    -- along with it.
    let run title evts = DeriveTest.extract DeriveTest.e_everything $
            DeriveTest.derive_tracks_tempo [(title, evts)]
    let inst = Just "i"

    let evt s d = (s, d, "", inst, [])
    equal (run ">i" [(0, 1, ""), (1, 2, ""), (3, 3, "")])
        ([evt 0 1, evt 1 2, evt 3 3], [])

    let (evts, logs) = run ">i" [(0, 1, "n +a 42")]
    equal evts []
    strings_like logs ["expected inst or attr"]

    let (evts, logs) = run ">i" [(0, 1, ")parse/error")]
    equal evts []
    strings_like logs ["parse error"]

    -- title error throws exception
    let (evts, logs) = run ">i $parse/err" [(0, 1, "")]
    equal evts []
    strings_like logs ["parse error"]

    -- comment only event is filtered out
    equal (run ">i" [(0, 1, "--")]) ([], [])
    equal (run ">" [(0, 1, "n >i +a")])
        ([(0, 1, "n >i +a", inst, ["a"])], [])
    equal (run ">i +a" [(0, 1, "")])
        ([(0, 1, "", inst, ["a"])], [])

    -- event overrides attrs
    equal (run "> +a" [(0, 1, "n =b"), (1, 1, "n -a")])
        ([ (0, 1, "n =b", Nothing, ["b"])
        , (1, 1, "n -a", Nothing, [])
        ], [])
    -- alternate syntax
    equal (run ">i" [(0, 1, ""), (1, 1, "n >i2 |")])
        ([ (0, 1, "", inst, [])
        , (1, 1, "n >i2 |", Just "i2", [])
        ], [])
