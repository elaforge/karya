-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.TScore.TScore_test where
import qualified Derive.TScore.TScore as TScore
import qualified Ui.Event as Event
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Util.Test


test_ui_state = do
    let f = fmap UiTest.extract_blocks . TScore.ui_state
    right_equal (f "top = \"block title\" [s r g]")
        [ ( "top -- block title"
          , [ (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4s"), (1, 0, "4r"), (2, 0, "4g")])
            ]
          )
        ]
    right_equal
            (f "top = %default-call [\"> | rh\" na din // >lh _ thom]")
        [ ( "top"
          , [ ("> | rh", [(0, 1, "na"), (1, 1, "din")])
            , (">lh", [(1, 1, "thom")])
            ]
          )
        ]

test_integrate = do
    let run state = Ui.exec state . TScore.integrate
    let extract = UiTest.extract_blocks
    let state = expect_right $ run Ui.empty "top = \"block title\" [s r g]"
    equal (extract state)
        [ ( "top -- block title"
          , [ (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4s"), (1, 0, "4r"), (2, 0, "4g")])
            ]
          )
        ]
    let tid = TScore.make_track_id (UiTest.bid "tscore/top") 1 True
    state <- return $ expect_right $ Ui.exec state $ do
        Ui.insert_event tid (Event.event 1 0 "5p")
        TScore.integrate "top = \"block title\" [s r s]"
    equal (extract state)
        [ ( "top -- block title"
          , [ (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4s"), (1, 0, "5p"), (2, 0, "4s")])
            ]
          )
        ]
