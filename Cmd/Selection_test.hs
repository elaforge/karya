module Cmd.Selection_test where

import Util.Test

import Ui.Types
import qualified Ui.Block as Block

import qualified Cmd.Selection as Selection


test_time_scroll_with_selection = do
    let mksel start end = Block.Selection 0 (TrackPos start) 1 (TrackPos end)
    let f a b c d = Selection.time_scroll_with_selection a b c d (TrackPos 1)

    equal (f (mksel 0 4) (mksel 0 4) 0 4) (TrackPos 0)
    -- 1 for scroll, 1 for extra
    equal (f (mksel 0 4) (mksel 0 5) 0 4) (TrackPos 2)

    -- Also works from the middle.
    equal (f (mksel 2 6) (mksel 2 6) 2 6) (TrackPos 2)
    equal (f (mksel 2 6) (mksel 1 6) 2 6) (TrackPos 0)
    equal (f (mksel 2 6) (mksel 2 7) 2 6) (TrackPos 4)
    -- If both expand, don't go anywhere.
    equal (f (mksel 2 6) (mksel 1 7) 2 6) (TrackPos 2)

test_track_scroll_with_selection = do
    let mksel start end =
            Block.Selection start (TrackPos 0) end (TrackPos 0)
    let widths = replicate 8 1
    let f a b c d = Selection.track_scroll_with_selection a b c d widths

    -- Scroll from the left side.
    equal (f (mksel 0 4) (mksel 0 4) 0 4) (TrackPos 0)
    equal (f (mksel 0 4) (mksel 0 5) 0 4) (TrackPos 1)
    equal (f (mksel 0 4) (mksel 0 6) 0 4) (TrackPos 2)

    -- Also works from the middle.
    equal (f (mksel 2 6) (mksel 2 6) 2 6) (TrackPos 2)
    equal (f (mksel 2 6) (mksel 1 6) 2 6) (TrackPos 1)
    equal (f (mksel 2 6) (mksel 0 6) 2 6) (TrackPos 0)
    equal (f (mksel 2 6) (mksel 2 7) 2 6) (TrackPos 3)
    equal (f (mksel 2 6) (mksel 2 8) 2 6) (TrackPos 4)
    -- Sensible even with out of range selection.
    equal (f (mksel 2 6) (mksel 2 9) 2 6) (TrackPos 4)

    equal (f (mksel 2 6) (mksel 2 7) 2.5 5.5) (TrackPos 4)

    -- If both expand, don't go anywhere.
    equal (f (mksel 2 6) (mksel 1 7) 2 6) (TrackPos 2)
