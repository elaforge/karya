module Derive.Call.Idiom.String_test where
import Util.Test
import qualified Util.Log as Log

import qualified Derive.Call.Idiom.String as String
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal


test_string = do
    let extract = DeriveTest.extract e_event Log.msg_string
        e_event e = (Score.event_start e,
            PitchSignal.unsignal_degree (Score.event_pitch e))
    let run p1 p2 p3 = extract $ DeriveTest.derive_tracks_with with_call
            [ ("> | guzheng 2 2 1", [(0, 5, ""), (5, 5, ""), (10, 5, "")])
            , ("*twelve", [(0, 0, p1), (5, 0, p2), (10, 0, p3)])
            ]
    let (res, logs) = run "4c" "2d" "2e"
    equal res (Right [(0, [(0, 60)])])
    strings_like logs ["event at 5s below", "event at 10s below"]

    -- All separate strings doesn't do anything interesting.
    equal (run "4c" "4d" "4e")
        (Right
            [ (0, [(0, 60)])
            , (5, [(5, 62)])
            , (10, [(10, 64)])
            ],
        [])

    equal (run "4c" "4c#" "4d")
        (Right
            [ (0, [(0, 60), (4, 60.5), (5, 61)]) -- bend up for attack
            , (5, [(5, 61), (12, 60.5), (13, 60)]) -- bend back down for release
            , (10, [(10, 62)])
            ],
        [])

with_call = CallTest.with_note_call "guzheng"
    (String.c_guzheng (map Pitch.Degree strings))

strings = [60, 62, 64, 67, 69]
