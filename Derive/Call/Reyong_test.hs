module Derive.Call.Reyong_test where
import qualified Data.Set as Set

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Integrate as Integrate
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import Types


test_integrate = do
    let run = extract . integrate . mktracks True
        extract (dres, cres) =
            (CmdTest.result_val cres, e_ui cres,
                DeriveTest.r_log_strings dres ++ e_logs cres)
        e_ui = snd . head . UiTest.extract_all_tracks
            . CmdTest.result_ui_state
        e_logs = map DeriveTest.show_log . CmdTest.result_logs

    let (val, tracks, logs) = run [(0, 2, "+byong", "1")]
    equal val (Right (Just ())) -- no error
    equal (take 6 tracks)
        [ ("> | < | realize-kilitan 1", [(0, 2, "+byong")])
        , ("*legong", [(0, 0, "1")])
        , (">", [(0, 2, "")]), ("*legong", [(0, 0, "`3.`")])
        , (">", [(0, 2, "")]), ("*legong", [(0, 0, "`6.`")])
        ]
    strings_like logs ["integrated *"]

    -- I just want to ascertain that the tracks were generated, not assert a
    -- bunch of specific stuff about what exactly was generated.
    prettyp (run [(0, 4, "", "1"), (4, 4, "", "2")])
    -- let (_val, tracks, _logs) = (run [(0, 4, "", "1"), (4, 4, "", "2")])
    -- equal (take 4 tracks)
    --     [ ("> | < | realize-kilitan 1", [(0, 1, ""), (4, 1, "")])
    --     , ("*legong", [(0, 0, "1"), (4, 0, "2")])
    --     , (">"
    --     ]

    prettyp (run [(0, 4, "", "1"), (8, 4, "", "2"), (12, 1, "+cek", ""),
        (13, 1, "+byut", ""), (14, 1, "+byong", "")])

integrate :: [UiTest.BlockSpec] -> (Derive.Result, CmdTest.Result ())
integrate [] = error "integrate got [] blocks"
integrate blocks@((block_name, _) : _) =
    (result, CmdTest.run ui_state CmdTest.empty_state $
        mapM_ (Integrate.integrate block_id) (Derive.r_integrated result))
    where
    result = DeriveTest.derive_blocks_with (block_damage blocks) blocks
    ui_state = Derive.state_ui $ Derive.state_constant $ Derive.r_state result
    block_id = fst $ UiTest.spec_block_id block_name

-- | If there's no block damage the integrate call doesn't collect anything.
block_damage :: [UiTest.BlockSpec] -> Derive.Deriver a -> Derive.Deriver a
block_damage blocks = DeriveTest.modify_constant $ \state -> state
    { Derive.state_score_damage = mempty
        { Derive.sdamage_blocks = Set.fromList block_ids }
    }
    where block_ids = map (fst . UiTest.spec_block_id . fst) blocks


test_realize = do
    let run pitches = DeriveTest.derive_blocks (mktracks False pitches)
        extract voice = first (extract_v voice) . DeriveTest.extract id
        extract_v voice = unwords . Seq.chunked 4 . concat
                . map DeriveTest.e_pitch . filter ((==voice) . event_voice)
        run1 = DeriveTest.extract
                (\e -> (DeriveTest.e_note e, event_voice e)) . run
        run2 voice = extract voice . run

    let (evts, logs) = run1 [(0, 4, "", "1")]
    equal (take 4 evts)
        [ ((1, 1, "`6.`"), 0), ((1, 1, "2"), 1)
        , ((1, 1, "6"), 2), ((1, 1, "`2^`"), 3)
        ]
    equal logs []
    equal (run2 1 [(0, 4, "", "1"), (4, 4, "", "2")])
        ("2232 3232", [])
    equal (run2 1 [(0, 8, "", "1"), (8, 4, "", "2")])
        ("2121 2232 3232", [])

event_voice :: Score.Event -> Int
event_voice = fromMaybe 0 . TrackLang.maybe_val TrackLang.v_voice
    . Score.event_environ

mktracks :: Bool -> [(ScoreTime, ScoreTime, String, String)]
    -> [UiTest.BlockSpec]
mktracks integrate pitches =
    [ ("b",
        [ ("> | " ++ title ++ "realize-kilitan 1", [(n, dur, text)
            | (n, dur, text, _) <- pitches])
        , ("*legong", [(n, 0, p) | (n, _, _, p) <- pitches, not (null p)])
        ])
    ]
    where title = if integrate then "< | " else ""
