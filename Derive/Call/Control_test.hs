module Derive.Call.Control_test where
import qualified Data.Map as Map

import Util.Test

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal


test_set = do
    equal (run [(0, 0, "1"), (1, 0, "0")]) (Right [Just [(0, 1), (1, 0)]])
    equal (run [(0, 0, "1"), (1, 0, "")]) (Right [Just [(0, 1)]])

test_linear = do
    -- make sure 0 arg variant works
    equal (run [(0, 0, "1"), (1, 0, "i"), (3, 0, "i 0")])
        (Right [Just [(0, 1), (1, 1), (2, 0.5), (3, 0)]])

test_exponential = do
    equal (run [(0, 0, "1"), (4, 0, "e 0")])
        (Right [Just [(0, 1), (1, 0.9375), (2, 0.75), (3, 0.4375), (4, 0)]])
    equal (run [(0, 0, "1"), (4, 0, "e 0 1")])
        (Right [Just [(0, 1), (1, 0.75), (2, 0.5), (3, 0.25), (4, 0)]])
    equal (run [(0, 0, "1"), (4, 0, "e 0 -1")])
        (Right [Just [(0, 1), (1, 0.75), (2, 0.5), (3, 0.25), (4, 0)]])
    equal (run [(0, 0, "1"), (4, 0, "e 0 -2")])
        (Right [Just [(0, 1), (1, 0.5),
            (2, 0.2928932188134524), (3, 0.1339745962155614), (4, 0)]])

run events = extract $ DeriveTest.derive_tracks_tempo
    [ (">", [(0, 1, "")])
    , ("cont", events)
    ]
    where
    extract = DeriveTest.extract_events_only (fmap Signal.unsignal
        . Map.lookup (Score.Control "cont") . Score.event_controls)
