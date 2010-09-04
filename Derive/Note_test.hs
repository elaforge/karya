module Derive.Note_test where
import Util.Test
import qualified Util.Log as Log

import qualified Derive.DeriveTest as DeriveTest


-- * derivers

test_c_note = do
    -- Test basic Derive.d_note_track plumbing and the note (null) deriver
    -- along with it.
    let run title evts = extract $
            DeriveTest.derive_tracks_tempo [(title, evts)]
        extract = DeriveTest.extract DeriveTest.e_everything Log.msg_string
    let inst = Just "i"

    let evt s d = (s, d, "", inst, [])
    equal (run ">i" [(0, 1, ""), (1, 2, ""), (3, 3, "")])
        (Right [evt 0 1, evt 1 2, evt 3 3], [])

    let (evts, logs) = run ">i" [(0, 1, "n +a 42")]
    equal evts (Right [])
    strings_like logs ["expected inst or attr"]

    let (evts, logs) = run ">i" [(0, 1, ")parse/error")]
    equal evts (Right [])
    strings_like logs ["parse error"]

    -- title error throws exception
    let (evts, logs) = run ">i $parse/err" [(0, 1, "")]
    equal evts (Right [])
    strings_like logs ["parse error"]

    -- comment only event is filtered out
    equal (run ">i" [(0, 1, "--")]) (Right [], [])
    equal (run ">" [(0, 1, "n >i +a")])
        (Right [(0, 1, "n >i +a", inst, ["a"])], [])
    equal (run ">i +a" [(0, 1, "")])
        (Right [(0, 1, "", inst, ["a"])], [])

    -- event overrides attrs
    equal (run "> +a" [(0, 1, "n =b"), (1, 1, "n -a")])
        (Right
            [ (0, 1, "n =b", Nothing, ["b"])
            , (1, 1, "n -a", Nothing, [])
            ],
        [])
    -- alternate syntax
    equal (run ">i" [(0, 1, ""), (1, 1, "n >i2 |")])
        (Right
            [ (0, 1, "", inst, [])
            , (1, 1, "n >i2 |", Just "i2", [])
            ],
        [])
