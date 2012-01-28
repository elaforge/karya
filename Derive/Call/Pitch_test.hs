module Derive.Call.Pitch_test where
import Util.Test
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.DeriveTest as DeriveTest


test_interpolated_transpose = do
    -- An even interpolation on an un-equal tempered scale should remain even
    -- after transposition.
    let scale = DeriveTest.mkscale "test" [("a", 1), ("b", 3), ("c", 4)]
    let run title = extract $ DeriveTest.derive_tracks_with
            (DeriveTest.with_scale scale)
            [ (title, [(0, 5, "")])
            , ("*test", [(0, 0, "a"), (4, 0, "i (b)")])
            ]
        extract = head . DeriveTest.extract_events DeriveTest.e_pitch
    equal (run ">") [(0, 1), (1, 1.5), (2, 2), (3, 2.5), (4, 3)]
    equal (run "> | %t-chromatic = 1")
        [(0, 3), (1, 3.25), (2, 3.5), (3, 3.75), (4, 4)]

test_transpose_out_of_range = do
    equal (run_with_title id ">" "twelve" [(0, "4c")])
        [(0, 60)]
    equal (run_with_title id "> | %t-chromatic = 10" "twelve" [(0, "4c")])
        [(0, 70)]
    equal (run_with_title id "> | %t-chromatic = -10" "twelve" [(0, "4c")])
        [(0, 50)]
    -- It's not actually an IO exception but that's how DeriveTest.e_pitch
    -- extractor treats an error in the pitch signal.
    throws (run_with_title id "> | %t-chromatic = 200" "twelve" [(0, "4c")])
        "note can't be transposed"

run_with_title with inst_title pitch_title pitches = extract $
    DeriveTest.derive_tracks_with with
        [ (inst_title, [(0, 5, "")])
        , ('*' : pitch_title, [(x, 0, n) | (x, n) <- pitches])
        ]
    where extract = head . DeriveTest.extract_events DeriveTest.e_pitch

test_neighbor = do
    equal (CallTest.run_pitch [(0, "n (4c) 1 2")])
        [(0, 61), (1, 60.5), (2, 60)]
    -- Both chromatic and diatonic literals.
    equal (CallTest.run_pitch [(0, "n (4c) 1c 1")]) [(0, 61), (1, 60)]
    equal (CallTest.run_pitch [(0, "n (4c) 1d 1")]) [(0, 62), (1, 60)]
