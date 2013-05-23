module Local.Instrument.Kontakt_test where
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Util.Test
import Util.Control

import Derive.Attrs
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Local.Instrument.Kontakt as Kontakt


test_kendang = do
    let run = DeriveTest.derive_tracks_with
            (DeriveTest.with_inst_db Kontakt.synth_descs)
        e_attrs = DeriveTest.extract $ \e ->
            (Score.event_start e, DeriveTest.e_inst e, Score.event_attributes e)
        strokes ns = [(t, 0, n) | (t, n) <- zip (Seq.range_ 0 1) ns]
    let wadon_inst = "kkt/kendang-wadon"
        lanang_inst = "kkt/kendang-lanang"
        kendang_inst = "kkt/kendang"
    let run_kendang suffix stroke_events = e_attrs $
            run [('>' : untxt kendang_inst ++ suffix, strokes stroke_events)]
    equal (run_kendang "" ["PL", "k", "P", "u", "U"])
        ( [ (0, kendang_inst, wadon <> plak)
          , (1, kendang_inst, wadon <> pak)
          , (2, kendang_inst, lanang <> pak)
          , (3, kendang_inst, wadon <> tut)
          , (4, kendang_inst, lanang <> tut)
          ]
        , []
        )
    let (events, logs) = run_kendang " | realize"
            ["PL", "k", "P", "t", "T", "t", "T"]
    equal logs []
    equal [(p, attrs) | (p, inst, attrs) <- events, inst == lanang_inst]
        [(1, ka <> soft), (2, pak), (3, ka <> soft), (4, pang), (5, pak),
            (6, pang)]
    equal [(p, attrs) | (p, inst, attrs) <- events, inst == wadon_inst]
        [(0, plak), (1, pak), (2, ka <> soft), (3, pang), (4, pak), (5, pang),
            (6, pak)]

    -- Soft attributes.
    let e_vel = DeriveTest.extract $ \e ->
            (Score.event_start e, Pretty.pretty (Score.event_attributes e),
                Score.initial_dynamic e)
    equal (e_vel $ run
            [ ('>' : untxt wadon_inst, strokes ["-", "+", ".", "P", "^"])
            , ("dyn", [(0, 0, "1")])
            ])
        ([(0, "+de+soft", 0.3), (1, "+de", 1), (2, "+ka+soft", 0.3),
            (3, "+pak", 1), (4, "+pak+soft", 0.3)], [])
