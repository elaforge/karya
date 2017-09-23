-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.BlockResize_test where

import qualified Util.CallStack as CallStack
import Util.Test
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Cmd.BlockResize as BlockResize


test_update_callers = do
    let run delta blocks = UiTest.extract_blocks $ UiTest.exec Ui.empty $ do
            UiTest.mkblocks blocks
            BlockResize.update_callers (UiTest.bid "low") 0 delta
    let low = ("low", [(">", [(0, 2, "")])])

    equal_b (run 0
        [ low
        , ("mid", [(">", [(0, 2, "low")])])
        , ("top", [(">", [(0, 2, "mid")])])
        ])
        [ low
        , ("mid", [(">", [(0, 2, "low")])])
        , ("top", [(">", [(0, 2, "mid")])])
        ]
    equal_b (run 2
        [ low
        , ("mid", [(">", [(0, 2, "low")])])
        , ("top", [(">", [(0, 2, "mid")])])
        ])
        [ low
        , ("mid", [(">", [(0, 4, "low")])])
        , ("top", [(">", [(0, 4, "mid")])])
        ]
    equal_b (run (-2)
        [ low
        , ("mid", [(">", [(0, 2, "low")])])
        , ("top", [(">", [(0, 2, "mid")])])
        ])
        [ low
        , ("mid", [(">", [(0, 0, "low")])])
        , ("top", [(">", [(0, 0, "mid")])])
        ]
    throws (run (-3)
        [ low
        , ("mid", [(">", [(0, 2, "low")])])
        , ("top", [(">", [(0, 2, "mid")])])
        ])
        "update delta -3t would invert event"

    -- Two calls in sequence accumulate delta.
    equal_b (run 2
        [ low
        , ("mid", [(">", [(0, 2, "low"), (2, 2, "low"), (4, 0, "")])])
        , ("top", [(">", [(0, 2, "mid")]), (">", [(1, 2, "mid")])])
        ])
        [ ("low", [(">", [(0, 2, "")])])
        , ("mid", [(">", [(0, 4, "low"), (4, 4, "low"), (8, 0, "")])])
        , ("top", [(">", [(0, 6, "mid")]), (">", [(1, 6, "mid")])])
        ]

    -- Not in sequence don't.
    equal_b (run 2
        [ low
        , ("mid", [(">", [(0, 2, "low")]), (">", [(0, 2, "low")])])
        , ("top", [(">", [(0, 2, "mid")])])
        ])
        [ low
        , ("mid", [(">", [(0, 4, "low")]), (">", [(0, 4, "low")])])
        , ("top", [(">", [(0, 4, "mid")])])
        ]

    -- Partial overlap.
    equal_b (run 2
        [ low
        , ("mid", [(">", [(0, 2, "low")]), (">", [(1, 2, "low")])])
        , ("top", [(">", [(0, 3, "mid")])])
        ])
        [ low
        , ("mid", [(">", [(0, 4, "low")]), (">", [(1, 4, "low")])])
        , ("top", [(">", [(0, 5, "mid")])])
        ]

equal_b :: CallStack.Stack => [UiTest.BlockSpec] -> [UiTest.BlockSpec]
    -> IO Bool
equal_b = equal_fmt UiTest.fmt_blocks
