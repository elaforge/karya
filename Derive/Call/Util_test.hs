{-# LANGUAGE ScopedTypeVariables #-}
module Derive.Call.Util_test where
import Util.Test
import qualified Ui.State as State
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang
import Types


test_random = do
    let run (seed :: Double) = DeriveTest.eval State.empty
            . Derive.with_val TrackLang.v_seed seed
        rand = Util.random :: Derive.Deriver Int
    equal (run 0 rand) (run 0 rand)
    check (run 0 rand /= run 1 rand)
    check $ run 2 (Internal.with_stack_region 0 1 rand)
        /= run 2 (Internal.with_stack_region 1 1 rand)
    equal (run 0 (Util.shuffle ['a'..'f'])) (Right "acdfbe") -- ya ya ya :)

test_c_equal = do
    -- Test the '=' call, but also test the special parsing Derive.Note deriver
    -- eval in general.
    let run title evts = DeriveTest.extract e_inst $
            DeriveTest.derive_tracks [(title, evts)]

    -- log stack should be at the track level
    let (evts, logs) = run "> | inst = inst" [(0, 1, "")]
    equal evts []
    strings_like logs ["expected Instrument"]

    -- only the event with the error is omitted
    let (evts, logs) = run ">" [(0, 1, "inst = inst |"), (1, 1, "")]
    equal evts [(1, "")]
    strings_like logs ["expected Instrument"]

    equal (run ">i" [(0, 1, ""), (1, 1, "inst = >i2 |"), (2, 1, "n >i3 |")])
        ([(0, "i"), (1, "i2"), (2, "i3")], [])

test_c_equal_note_transformer = do
    let run events = DeriveTest.extract e_inst $
            DeriveTest.linear_derive_tracks id
                [ (">", events)
                , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
                ]
    equal (run []) ([(0, ""), (1, ""), (2, "")], [])
    equal (run [(0, 2, "inst = >i")]) ([(0, "i"), (1, "i"), (2, "")], [])
    equal (run [(0, 3, "inst = >i")]) ([(0, "i"), (1, "i"), (2, "i")], [])
    equal (run [(0, 1, "inst = >i1"), (1, 1, "inst = >i2")])
        ([(0, "i1"), (1, "i2"), (2, "")], [])

e_inst :: Score.Event -> (RealTime, String)
e_inst e = (Score.event_start e, Score.inst_name (Score.event_instrument e))
