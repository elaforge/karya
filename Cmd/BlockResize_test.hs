-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.BlockResize_test where
import           GHC.Stack (HasCallStack)

import qualified Cmd.BlockResize as BlockResize
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Util.Test


test_update_callers :: Test
test_update_callers = do
    let run delta blocks = UiTest.extractBlocks $ UiTest.exec Ui.empty $ do
            UiTest.mkblocks blocks
            BlockResize.updateCallers (UiTest.bid "low") 0 delta
    let low = ("low", [(">", [(0, 2, "")])])
        low2 = ("low", [(">", [(2, 2, "")])])

    equalB (run 0
        [ low
        , ("mid", [(">", [(0, 2, "low")])])
        , ("top", [(">", [(0, 2, "mid")])])
        ])
        [ low
        , ("mid", [(">", [(0, 2, "low")])])
        , ("top", [(">", [(0, 2, "mid")])])
        ]
    equalB (run 2
        [ low
        , ("mid", [(">", [(0, 2, "low")])])
        , ("top", [(">", [(0, 2, "mid")])])
        ])
        [ low2
        , ("mid", [(">", [(0, 4, "low")])])
        , ("top", [(">", [(0, 4, "mid")])])
        ]
    equalB (run (-2)
        [ low
        , ("mid", [(">", [(0, 2, "low")])])
        , ("top", [(">", [(0, 2, "mid")])])
        ])
        [ ("low", [(">", [])])
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
    equalB (run 2
        [ low
        , ("mid", [(">", [(0, 2, "low"), (2, 2, "low"), (4, 0, "")])])
        , ("top", [(">", [(0, 2, "mid")]), (">", [(1, 2, "mid")])])
        ])
        [ low2
        , ("mid", [(">", [(0, 4, "low"), (4, 4, "low"), (8, 0, "")])])
        , ("top", [(">", [(0, 6, "mid")]), (">", [(1, 6, "mid")])])
        ]

    -- Not in sequence don't.
    equalB (run 2
        [ low
        , ("mid", [(">", [(0, 2, "low")]), (">", [(0, 2, "low")])])
        , ("top", [(">", [(0, 2, "mid")])])
        ])
        [ low2
        , ("mid", [(">", [(0, 4, "low")]), (">", [(0, 4, "low")])])
        , ("top", [(">", [(0, 4, "mid")])])
        ]

    -- Partial overlap.
    equalB (run 2
        [ low
        , ("mid", [(">", [(0, 2, "low")]), (">", [(1, 2, "low")])])
        , ("top", [(">", [(0, 3, "mid")])])
        ])
        [ low2
        , ("mid", [(">", [(0, 4, "low")]), (">", [(1, 4, "low")])])
        , ("top", [(">", [(0, 5, "mid")])])
        ]

    -- Children also move.
    equalB (run 2
        [ ("low", [(">", [(0, 2, "")]), ("*", [(0, 0, "3c")])])
        , ("mid", [(">", [(0, 2, "low"), (2, 0, "")]), ("*", [(2, 0, "4c")])])
        ])
        [ ("low", [(">", [(2, 2, "")]), ("*", [(2, 0, "3c")])])
        , ("mid", [(">", [(0, 4, "low"), (4, 0, "")]), ("*", [(4, 0, "4c")])])
        ]

test_update_rulers :: Test
test_update_rulers = do
    let run pos delta blocks = UiTest.eRulers $ UiTest.exec Ui.empty $ do
            UiTest.mkblocks blocks
            BlockResize.updateCallersRulers (UiTest.bid "low") pos delta
    let blocks mid top =
            [ ("low=ruler", [(">", [(0, 4, "")])])
            , ("mid=ruler", [(">", mid)])
            , ("top=ruler", [(">", top)])
            ]
    equal (run 0 0 (blocks [(0, 4, "low")] [(0, 4, "mid")]))
        [ ("low", "1 .2 .3 .4 2")
        , ("mid", "1 .2 .3 .4 2")
        , ("top", "1 .2 .3 .4 2")
        ]
    equal (run 0 4 (blocks [(0, 4, "low")] [(0, 4, "mid")]))
        [ ("low", "1 .2 .3 .4 2 .2 .3 .4 3")
        , ("mid", "1 .2 .3 .4 2 .2 .3 .4 3")
        , ("top", "1 .2 .3 .4 2 .2 .3 .4 3")
        ]
    -- TODO more tests some day

-- * util

equalB :: HasCallStack => [UiTest.BlockSpec] -> [UiTest.BlockSpec] -> Test
equalB = equalFmt UiTest.fmtBlocks
