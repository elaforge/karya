module Local.Instrument.Kontakt_test where
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Util.Test

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Local.Instrument.Kontakt as Kontakt


test_kendang = do
    let run = DeriveTest.derive_tracks_with
            (DeriveTest.with_inst_db Kontakt.synth_descs)
        e_attrs = DeriveTest.extract DeriveTest.e_everything
        strokes ns = [(t, t, n) | (t, n) <- zip (Seq.range_ 0 1) ns]
    let text = "<eval_one: >"
        wadon = "kkt/kendang-wadon"
        lanang = "kkt/kendang-lanang"
    equal (e_attrs $ run
            [(">kkt/kendang-composite", strokes ["PL", "k", "P", "u", "U"])])
        ( [ (0, 0, text, wadon, ["plak"])
          , (1, 0, text, wadon, ["pak"])
          , (2, 0, text, lanang, ["pak"])
          , (3, 0, text, wadon, ["tut"])
          , (4, 0, text, lanang, ["tut"])
          ]
        , []
        )

    -- Soft attributes.
    let e_vel = DeriveTest.extract $ \e ->
            (Score.event_start e, Pretty.pretty (Score.event_attributes e),
                Score.initial_dynamic e)
    equal (e_vel $ run
            [ ('>':wadon, strokes ["-", "+", ".", "P", "^"])
            , ("dyn", [(0, 0, "1")])
            ])
        ([(0, "+de+soft", 0.3), (1, "+de", 1), (2, "+ka", 1), (3, "+pak", 1),
            (4, "+pak+soft", 0.3)], [])
