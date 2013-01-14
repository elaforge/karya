module Derive.Call.Ornament_test where
import Util.Test
import qualified Ui.State as State
import qualified Derive.Call.Ornament as Ornament
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


test_track = do
    equal (DeriveTest.extract DeriveTest.e_twelve $ DeriveTest.derive_tracks
        [ (">", [(1, 1, "`mordent`")])
        , ("*", [(0, 0, "4c")])
        ])
        (["4c", "4d", "4c"], [])

test_mordent = do
    let run = DeriveTest.extract DeriveTest.e_twelve . DeriveTest.derive_tracks
    equal (run
        [ (">", [(1, 1, "`mordent`")])
        , ("*", [(0, 0, "4c")])
        ])
        (["4c", "4d", "4c"], [])
    equal (run
        [ (">", [(1, 1, "`mordent`")])
        , ("*", [(0, 0, "4e")])
        ])
        (["4e", "4f", "4e"], [])

    let f = Ornament.mordent
        run = DeriveTest.run_events extract
            . DeriveTest.run State.empty
            . Util.with_pitch (DeriveTest.mkpitch "a")
            . Util.with_dynamic 1
            . Derive.with_val (TrackLang.Symbol "grace-dur") (1 :: Double)
            . Derive.with_val (TrackLang.Symbol "grace-overlap") (0.5 :: Double)
        extract e = (Score.event_start e, DeriveTest.e_twelve e,
            DeriveTest.e_control "dyn" e)
    equal (run (f (4, 1) 0.25 (Pitch.Chromatic 1))) $ Right
        ( [ (2, "4c", [(0, 0.25)])
          , (3, "4c#", [(0, 0.25)])
          , (4, "4c", [(0, 1)])
          ]
        , []
        )
    -- It's in RealTime, so it's not affected by the tempo.
    equal (run (Derive.d_stretch 2 (f (4, 1) 0.25 (Pitch.Chromatic (-1))))) $
        Right
            ( [ (6, "4c", [(0, 0.25)])
              , (7, "3b", [(0, 0.25)])
              , (8, "4c", [(0, 1)])
              ]
            , []
            )

test_grace = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks
        extract e = (DeriveTest.e_note2 e, Score.initial_dynamic e)
        title = "> | grace-dur = 1 | grace-overlap = .5"
        dur = 1
        overlap = 0.5
    equal (run
        [ (title, [(0, 1, "g (4a) (4b)")])
        , ("*", [(0, 0, "4c")])
        ])
        ( [ ((0-dur*2, dur+overlap, "4a"), 0.5)
          , ((0-dur, dur+overlap, "4b"), 0.5)
          , ((0, 1, "4c"), 1)
          ]
        , []
        )
    equal (run
        [ (title, [(1, 1, "g 1 (4b)")])
        , ("*", [(1, 0, "4c")])
        ])
        ([((1-dur, dur+overlap, "4b"), 1), ((1, 1, "4c"), 1)], [])

    -- Ensure the grace-dyn default is picked up too.
    equal (run
        [ (title ++ "| grace-dyn = 1", [(1, 1, "g (4b)")])
        , ("*", [(0, 0, "4c")])
        ])
        ([((0, 1.5, "4b"), 1), ((1, 1, "4c"), 1)], [])
