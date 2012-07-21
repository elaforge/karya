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
    let f = first (map extract) . integrate
        integrate = Convert.integrate lookup_scale lookup_attrs Nothing
        lookup_attrs = const $ Map.fromList [(Attrs.plak, "plak")]
        extract (Convert.Track title events) =
            (title, map UiTest.extract_event events)
        inst = Score.Instrument "inst"
        event = DeriveTest.mkevent
    equal (f [event (0, 1, "a", [], inst)])
        ([(">inst", [(0, 1, "")]), ("*twelve", [(0, 0, "4c")])], [])
    -- No pitch track, has a control track.
    equal (f [(event (0, 1, "a", [("dyn", [(0, 0), (2, 1)])], inst))
            { Score.event_pitch = Pitches.signal Twelve.scale [] }])
        ( [ (">inst", [(0, 1, "")])
          , ("dyn", [(0, 0, "`0x`00"), (2, 0, "`0x`ff")])
          ]
        , []
        )
    -- Attributes get mapped back to their call.
    equal (f [(event (0, 1, "a", [], inst))
            { Score.event_attributes = Attrs.plak }])
        ( [(">inst", [(0, 1, "plak")]), ("*twelve", [(0, 0, "4c")])]
        , []
        )

lookup_scale :: Pitch.ScaleId -> Maybe Scale.Scale
lookup_scale = flip Map.lookup Scale.All.scales
