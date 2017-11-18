-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Integrate.Convert_test where
import qualified Data.Map as Map

import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.Integrate.Convert as Convert
import qualified Derive.Attrs as Attrs
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import Global


test_integrate = do
    let f = first (map extract . concatMap flatten) . integrate
        integrate = Convert.integrate (lookup_attrs, Pitch.twelve) tracknums
        plak = Attrs.attr "plak"
        lookup_attrs = const $ Map.fromList [(plak, "plak")]
        tracknums = Map.fromList [(UiTest.mk_tid n, n) | n <- [1..10]]
        flatten (note, controls) = note : controls
        extract (Convert.Track title events) =
            (title, map UiTest.extract_event events)
    equal (f [event (0, 1, "4c", [], inst)])
        ([(">inst", [(0, 1, "")]), ("*", [(0, 0, "4c")])], [])
    -- No pitch track, has a control track.
    equal (f [no_pitch (0, 1, [("dyn", [(0, 0), (2, 1)])])])
        ( [ (">inst", [(0, 1, "")])
          , ("dyn", [(0, 0, "`0x`00"), (2, 0, "`0x`ff")])
          ]
        , []
        )
    -- Attributes get mapped back to their call.
    let set = Score.modify_attributes . const
    equal (f [set plak (no_pitch (0, 1, []))])
        ( [(">inst", [(0, 1, "plak")])]
        , []
        )

    -- Duplicate controls are suppressed.
    equal (f
            [ no_pitch (0, 1, [("dyn", [(0, 1), (1, 1), (2, 0)])])
            , no_pitch (3, 1, [("dyn", [(3, 0)])])
            ])
        ( [ (">inst", [(0, 1, ""), (3, 1, "")])
          , ("dyn", [(0, 0, "`0x`ff"), (2, 0, "`0x`00")])
          ]
        , []
        )
    -- But duplicate pitches are only suppressed when they're on the same note.
    equal (f
            [ pitches (0, 1, [(0, "4c"), (1, "4c"), (2, "4d")])
            , pitches (3, 1, [(3, "4d")])
            ])
        ( [ (">inst", [(0, 1, ""), (3, 1, "")])
          , ("*", [(0, 0, "4c"), (2, 0, "4d"), (3, 0, "4d")])
          ]
        , []
        )
    where
    no_pitch (start, dur, controls) =
        Score.set_pitch mempty $ event (start, dur, "4c", controls, inst)
    pitches (start, dur, pitches) =
        Score.set_pitch (DeriveTest.psignal pitches) $
            event (start, dur, "4c", [], inst)
    event = DeriveTest.mkevent
    inst = "inst"

test_split_overlapping = do
    let f = map (map extract) . Convert.split_overlapping . map mkevent
        mkevent (s, e) =
            DeriveTest.mkevent (s, e, "4c", [], Score.empty_instrument)
        extract e = (Score.event_start e, Score.event_duration e)
    equal (f [(0, 1), (1, 1)]) [[(0, 1), (1, 1)]]
    equal (f [(0, 1), (0.5, 1), (1, 1)]) [[(0, 1), (1, 1)], [(0.5, 1)]]
