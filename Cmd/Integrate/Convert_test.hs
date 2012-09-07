module Cmd.Integrate.Convert_test where
import qualified Data.Map as Map

import Util.Control
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.Integrate.Convert as Convert
import qualified Derive.Attrs as Attrs
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch


test_integrate = do
    let f = first (map extract . concatMap flatten) . integrate
        integrate = Convert.integrate lookup_scale lookup_attrs Nothing
        lookup_attrs = const $ Map.fromList [(Attrs.plak, "plak")]
        flatten (note, controls) = note : controls
        extract (Convert.Track title events) =
            (title, map UiTest.extract_event events)
    equal (f [event (0, 1, "a", [], inst)])
        ([(">inst", [(0, 1, "")]), ("*twelve", [(0, 0, "4c")])], [])
    -- No pitch track, has a control track.
    equal (f [no_pitch (0, 1, [("dyn", [(0, 0), (2, 1)])])])
        ( [ (">inst", [(0, 1, "")])
          , ("dyn", [(0, 0, "`0x`00"), (2, 0, "`0x`ff")])
          ]
        , []
        )
    -- Attributes get mapped back to their call.
    let set = Score.modify_attributes . const
    equal (f [set Attrs.plak (no_pitch (0, 1, []))])
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
            [ pitches (0, 1, [(0, "a"), (1, "a"), (2, "b")])
            , pitches (3, 1, [(3, "b")])
            ])
        ( [ (">inst", [(0, 1, ""), (3, 1, "")])
          , ("*twelve", [(0, 0, "4c"), (2, 0, "4c#"), (3, 0, "4c#")])
          ]
        , []
        )
    where
    no_pitch (start, dur, controls) = (event (start, dur, "a", controls, inst))
        { Score.event_pitch = Pitches.signal Twelve.scale [] }
    pitches (start, dur, pitches) = (event (start, dur, "a", [], inst))
        { Score.event_pitch = DeriveTest.pitch_signal pitches }
    event = DeriveTest.mkevent
    inst = Score.Instrument "inst"

lookup_scale :: Pitch.ScaleId -> Maybe Scale.Scale
lookup_scale = flip Map.lookup Scale.All.scales
