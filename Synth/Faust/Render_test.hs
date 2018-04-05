-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Faust.Render_test where
import qualified Data.Map as Map
import Util.Test
import qualified Synth.Faust.Render as Render
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal


test_gateBreakpoints = do
    let f = Render.gateBreakpoints . map (uncurry (Note.note "" ""))
    equal (f []) []
    equal (f [(0, 1)]) [(0, 0), (0, 1), (1, 0)]
    equal (f [(0, 1), (1, 1)]) [(0, 0), (0, 1), (1, 0), (1, 0), (1, 1), (2, 0)]

    -- -- TODO if I enable gate combining
    -- equal (f [(0, 1)]) [(0, 0), (0, 1), (1, 1), (1, 0)]
    -- equal (f [(0, 1), (1, 1)]) [(0, 0), (0, 1), (2, 1), (2, 0)]
    -- equal (f [(0, 1), (1, 1), (3, 1)])
    --     [(0, 0), (0, 1), (2, 1), (2, 0), (3, 0), (3, 1), (4, 1), (4, 0)]
    -- equal (f [(1, 1), (3, 1)])
    --     [(1, 0), (1, 1), (2, 1), (2, 0), (3, 0), (3, 1), (4, 1), (4, 0)]

test_controlBreakpoints = do
    let f = Render.controlBreakpoints "c" . map make
        make (s, e, cs) = (Note.note "" "" s e)
            { Note.controls = Map.singleton "c" (Signal.from_pairs cs)
            }
    equal (f []) []
    equal (f [(0, 1, []), (1, 1, [])]) [(0, 0), (1, 0)]
    -- Audio.linear should optimize away duplicate and flat breakpoints.
    equal (f [(0, 1, [(0, 1)]), (1, 1, [(0, 1)])]) [(0, 1), (1, 1), (1, 1)]

    equal (f [(0, 1, [(0, 1)])]) [(0, 1)]
    -- Signals stay constant.
    equal (f [(0, 1, [(0, 1)]), (2, 1, [(2, 2)])])
        [(0, 1), (2, 1), (2, 2)]
    -- Notes clip other notes.
    equal (f [(0, 1, [(0, 0), (6, 6)]), (2, 1, [(0, 0)])])
        [(0, 0), (2, 2), (2, 0)]
