module Derive.Call.Reyong_test where
import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Util.Test

import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Reyong as Reyong
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Score as Score


-- TODO how to test integration?

test_realize = do
    let run pitches = DeriveTest.derive_blocks
            [ ("b=ruler",
                [ ("> | scale = *legong | realize-kilitan 1", [(n, 1, "")
                    | (n, _) <- pitches])
                , ("*legong", [(n, 0, p) | (n, p) <- pitches])
                ])
            ]
        extract voice = first (extract_v voice) . DeriveTest.extract id
        extract_v voice =
                unwords . Seq.chunked 4 . concat
                . map DeriveTest.e_pitch
                . filter (Score.has_attribute voice)
        run1 = DeriveTest.extract
                (\e -> (DeriveTest.e_note e, DeriveTest.e_attributes e)) . run
        run2 voice = extract voice . run

    let (evts, logs) = run1 [(0, "1")]
    equal (take 4 evts)
        [ ((1, 1, "`6.`"), "+voice1"), ((1, 1, "2"), "+voice2")
        , ((1, 1, "6"), "+voice3"), ((1, 1, "`2^`"), "+voice4")
        ]
    equal logs []

    equal (run2 Attrs.voice2 [(0, "1"), (4, "2")])
        ("2232 3232", [])
    equal (run2 Attrs.voice2 [(0, "1"), (8, "2")])
        ("2121 2232 3232", [])
    pprint (run2 Attrs.voice2 [(0, "2"), (8, "3")])

test_extract_pokok = do
    let f beats =
            map Pretty.pretty . Reyong.extract_pokok (Seq.range' 0 beats 1)
                . map mkevent
        mkevent (start, dur, p) = DeriveTest.mkevent_scale Legong.scale
            (start, dur, p, [], Score.empty_inst)
    equal (f 3 [(0, 1, "1"), (1, 1, "2")]) ["0I", "0O", "0O"]
    -- It's ok if the note is a little off the beat.
    equal (f 3 [(0, 1, "1"), (1.05, 1, "2")]) ["0I", "0O", "0O"]
    -- Middle pitch isn't counted.
    equal (f 3 [(0, 1, "1"), (0.5, 1, "3"), (1, 1, "2")]) ["0I", "0O", "0O"]
