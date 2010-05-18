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

run events = extract $ DeriveTest.derive_tracks_tempo
    [ (">", [(0, 1, "")])
    , ("cont", events)
    ]
    where
    extract = DeriveTest.extract_events_only (fmap Signal.unsignal
        . Map.lookup (Score.Control "cont") . Score.event_controls)
