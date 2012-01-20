module Derive.Call.Ornament_test where
import Util.Test
import qualified Ui.State as State
import qualified Derive.Call.Ornament as Ornament
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime


test_track = do
    let ex (_, pitch, _) = pitch
    equal (DeriveTest.extract (ex . extract) $ DeriveTest.derive_tracks
        [ (">", [(1, 1, "`mordent`")])
        , ("*twelve", [(0, 0, "4c")])
        ])
        ([[(0, 60)], [(0, 61)], [(0, 60)]], [])

test_mordent = do
    let f = Ornament.mordent (RealTime.seconds 1)
        run = DeriveTest.run_events extract
            . DeriveTest.run State.empty
            . Util.with_pitch (DeriveTest.mkpitch "a")
            . Util.with_velocity 1
    equal (run (f (4, 1) 0.25 (Pitch.Chromatic 1))) $ Right
        ([(2, [(0, 60)], Just [(0, 0.25)])
        , (3, [(0, 61)], Just [(0, 0.25)])
        , (4, [(0, 60)], Just [(0, 1)])
        ], [])
    -- It's in RealTime
    equal (run (Derive.d_stretch 2 (f (4, 1) 0.25 (Pitch.Chromatic (-1))))) $
        Right
            ([(6, [(0, 60)], Just [(0, 0.25)])
            , (7, [(0, 59)], Just [(0, 0.25)])
            , (8, [(0, 60)], Just [(0, 1)])
            ], [])

extract e = (Score.event_start e, DeriveTest.e_pitch e,
    DeriveTest.e_control "vel" e)
