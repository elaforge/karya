module Local.Instrument.Kontakt_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Local.Instrument.Kontakt as Kontakt


test_kendang = do
    let run = extract . DeriveTest.derive_tracks_with
            (DeriveTest.with_inst_db Kontakt.synth_descs)
        extract = DeriveTest.extract DeriveTest.e_everything
    let strokes = ["PL", "k", "P", "u", "U"]
    let text = "<eval_one: >"
        wadon = "kkt/kendang-wadon"
        lanang = "kkt/kendang-lanang"
    equal (run [(">kkt/kendang-composite",
            [(t, t, n) | (t, n) <- zip (Seq.range_ 0 1) strokes])])
        ( [ (0, 0, text, wadon, ["plak"])
          , (1, 0, text, wadon, ["pak"])
          , (2, 0, text, lanang, ["pak"])
          , (3, 0, text, wadon, ["tut"])
          , (4, 0, text, lanang, ["tut"])
          ]
        , []
        )
