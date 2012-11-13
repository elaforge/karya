module Derive.Call.Attribute_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Derive.DeriveTest as DeriveTest


test_legato = do
    let run = DeriveTest.extract extract . DeriveTest.linear_derive_tracks id
        extract e = (s, d, p, a)
            where
            ((s, d, p), a) = (DeriveTest.e_note2 e, DeriveTest.e_attributes e)
    let (evts, logs) = run
            [ (">", [(1, 3, "( .1")])
            , (">", [(n, 1, "") | n <- Seq.range 0 4 1])
            , ("*", [(n, 0, p) | (n, p) <-
                zip (Seq.range_ 0 1) ["4a", "4b", "4c", "4d", "4e"]])
            ]
    equal logs []
    equal evts
        [ (0, 1, "4a", "-")
        , (1, 1.1, "4b", "+legato")
        , (2, 1.1, "4c", "+legato")
        , (3, 1, "4d", "+legato")
        , (4, 1, "4e", "-")
        ]

    -- Legato events are extended to always overlap.
    let (evts, logs) = run
            [ (">", [(0, 4, "( .1")])
            , (">", [(0, 1, ""), (2, 1, ""), (4, 1, "")])
            ]
    equal logs []
    equal evts
        [ (0, 2.1, "?", "+legato")
        , (2, 1, "?", "+legato")
        , (4, 1, "?", "-")
        ]
