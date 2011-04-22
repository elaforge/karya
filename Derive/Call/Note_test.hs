module Derive.Call.Note_test where

import Util.Test

import qualified Derive.DeriveTest as DeriveTest


test_clip = do
    let extract = DeriveTest.extract DeriveTest.e_event
        run tracks = extract $ DeriveTest.derive_blocks tracks
    equal (run [("b1", [(">", [(0, 1, "clip b2")])])])
        ([], ["DeriveError: block not found: b2"])
    -- make sure out of range notes are clipped
    equal (run
        [ ("b1", [(">", [(0, 1, "clip b2")])])
        , ("b2", [(">", [(0, 1, ""), (1, 1, "")])])
        ])
        ([(0, 1, "")], [])
    -- the tempo of the block is not affected by the duration of the event
    equal (run
        [ ("b1", [(">", [(1, 2, "clip b2")])])
        , ("b2", [(">", [(0, 1, ""), (1, 1, "")])])
        ])
        ([(1, 1, ""), (2, 1, "")], [])
