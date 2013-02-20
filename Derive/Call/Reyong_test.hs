module Derive.Call.Reyong_test where
import qualified Data.Set as Set

import Util.Control
import qualified Util.Debug as Debug
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Integrate as Integrate
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Reyong as Reyong
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Score as Score

import Types


test_integrate = do
    let run = extract . integrate . mktracks True
        extract (dres, cres) =
            (CmdTest.result_val cres, e_ui cres,
                DeriveTest.r_log_strings dres ++ e_logs cres)
        e_ui = snd . head . UiTest.extract_all_tracks
            . CmdTest.result_ui_state
        e_logs = map DeriveTest.show_log . CmdTest.result_logs
    let (_val, tracks, _logs) = run [(0, "+byong", "1")]
    equal (take 6 tracks)
        [ ("> | < | realize-kilitan 1", [(0, 1, "+byong")])
        , ("*legong", [(0, 0, "1")])
        , (">", [(0, 1, "")]), ("*legong", [(0, 0, "`3.`")])
        , (">", [(0, 1, "")]), ("*legong", [(0, 0, "`6.`")])
        ]
    -- I just want to ascertain that the tracks were generated, not assert a
    -- bunch of specific stuff about what exactly was generated.
    prettyp (run [(0, "", "1"), (4, "", "2")])
    -- let (_val, tracks, _logs) = (run [(0, "", "1"), (4, "", "2")])
    -- equal (take 4 tracks)
    --     [ ("> | < | realize-kilitan 1", [(0, 1, ""), (4, 1, "")])
    --     , ("*legong", [(0, 0, "1"), (4, 0, "2")])
    --     , (">"
    --     ]

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
        extract_v voice =
                unwords . Seq.chunked 4 . concat
                . map DeriveTest.e_pitch
                . filter (Score.has_attribute voice)
        run1 = DeriveTest.extract
                (\e -> (DeriveTest.e_note e, DeriveTest.e_attributes e)) . run
        run2 voice = extract voice . run

    let (evts, logs) = run1 [(0, "", "1")]
    equal (take 4 evts)
        [ ((1, 1, "`6.`"), "+voice1"), ((1, 1, "2"), "+voice2")
        , ((1, 1, "6"), "+voice3"), ((1, 1, "`2^`"), "+voice4")
        ]
    equal logs []

    equal (run2 Attrs.voice2 [(0, "", "1"), (4, "", "2")])
        ("2232 3232", [])
    equal (run2 Attrs.voice2 [(0, "", "1"), (8, "", "2")])
        ("2121 2232 3232", [])

test_extract_pokok = do
    let f beats =
            map Pretty.pretty . Reyong.extract_pokok (Seq.range' 0 beats 1)
                . map mkevent
        mkevent (start, dur, p) = DeriveTest.mkevent_scale Legong.scale
            (start, dur, p, [], Score.empty_inst)
    equal (f 3 [(0, 1, "1"), (1, 1, "2")]) ["0I", "0O", "0O"]
    -- It's ok if the note is a little off the beat.
    equal (f 3 [(0, 1, "1"), (1.05, 1, "2")]) ["0I", "0O", "0O"]
    -- Middle pitch isn't counted.
    equal (f 3 [(0, 1, "1"), (0.5, 1, "3"), (1, 1, "2")]) ["0I", "0O", "0O"]

mktracks :: Bool -> [(ScoreTime, String, String)] -> [UiTest.BlockSpec]
mktracks integrate pitches =
    [ ("b",
        [ ("> | " ++ title ++ "realize-kilitan 1", [(n, 1, text)
            | (n, text, _) <- pitches])
        , ("*legong", [(n, 0, p) | (n, _, p) <- pitches, not (null p)])
        ])
    ]
    where title = if integrate then "< | " else ""
