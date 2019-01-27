-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.TScore.TScore_test where
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Ruler.Meter as Meter
import qualified Derive.TScore.TScore as TScore
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.GenId as GenId
import qualified Ui.Style as Style
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_ui_state = do
    let f = fmap UiTest.extract_blocks . TScore.ui_state get_ext_dur
    right_equal (f "top = \"block title\" [s r g]")
        [ ( "top -- block title"
          , [ (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4s"), (1, 0, "4r"), (2, 0, "4g")])
            ]
          )
        ]
    right_equal (f "top = %default-call [\"> | rh\" na din // >lh _ thom]")
        [ ( "top"
          , [ ("> | rh", [(0, 1, "na"), (1, 1, "din")])
            , (">lh", [(1, 1, "thom")])
            ]
          )
        ]
    -- sub-blocks
    right_equal (f "top = [s [r [g m]/]/]")
        [ ("top", UiTest.note_track [(0, 1, "4s"), (1, 1, "-1c1 -- 4s")])
        , ("top-1c1", UiTest.note_track [(0, 1, "4r"), (1, 1, "-1c1 -- 4r")])
        , ("top-1c1-1c1", UiTest.note_track [(0, 1, "4g"), (1, 1, "4m")])
        ]

test_call_duration = do
    let f = fmap UiTest.extract_blocks . TScore.ui_state get_ext_dur
    let b_block = ("b", UiTest.note_track [(0, 1, "4s"), (1, 1, "4r")])
    right_equal (f "a = [b/]\nb = [s r]")
        [ ("a", [(">", [(0, 1, "b")])])
        , b_block
        ]
    right_equal (f "a = [b/0]\nb = [s r]")
        [ ("a", [(">", [(0, 2, "b")])])
        , b_block
        ]
    -- CallDuration is carried like other durs.
    right_equal (f "a = %default-call [b0 b]\nb = [s r]")
        [ ("a", [(">", [(0, 2, "b"), (2, 2, "b")])])
        , b_block
        ]
    -- The default dur carries past CallDuration.
    right_equal (f "a = [b/0 s]\nb = [s r]")
        [ ("a",
            [ (">", [(0, 2, "b"), (2, 1, "")])
            , ("*", [(2, 0, "4s")])
            ])
        , b_block
        ]
    right_equal (f "a = [b/0 _ b/]\nb = [s r]")
        [ ("a", [(">", [(0, 2, "b"), (3, 2, "b")])])
        , b_block
        ]
    -- Works for sub-blocks.
    right_equal (f "a = [[s r]/0 g]")
        [ ("a", UiTest.note_track [(0, 2, "-1c1 --"), (2, 1, "4g")])
        , ("a-1c1", UiTest.note_track1 ["4s", "4r"])
        ]

test_ext_call_duration = do
    let f blocks source = extract $
            CmdTest.run_blocks blocks (TScore.cmd_integrate source)
        extract = CmdTest.trace_logs
            . CmdTest.extract_ui_state UiTest.extract_blocks
    let blocks = [("top=ruler", UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])]
        top = ("top", UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])

    -- pprint $ Ui.state_blocks $ snd $ UiTest.run Ui.empty $
    --     UiTest.mkblocks blocks

    -- It uses the root block's namespace, not tscore.
    right_equal (f blocks "a = [top/0 s1]")
        [ top
        , ("a", UiTest.note_track [(0, 2, "top --"), (2, 1, "4s")])
        ]

    -- TODO
    -- -- Use block title for context.
    -- right_equal (f blocks "a = \"import india.mridangam\" [\">\" \"4n\"/0 s1]")
    --     [ top
    --     , ("a -- import india.mridangam", [])
    --     ]

e_events :: Ui.State -> [[Event.Event]]
e_events = map (Events.ascending . Track.track_events) . Map.elems
    . Ui.state_tracks

test_integrate = do
    let run state = Ui.exec state . TScore.integrate get_ext_dur
    let extract = UiTest.extract_blocks
    let state = expect_right $ run Ui.empty "top = \"block title\" [s r g]"
    equal (extract state)
        [ ("top -- block title",
            UiTest.note_track [(0, 1, "4s"), (1, 1, "4r"), (2, 1, "4g")])
        ]

    -- Make sure style and ruler are as expected.
    equal (map (map Event.style) (e_events state)) $ map (map Style.StyleId)
        [ [1, 1, 1]
        , [1, 1, 1]
        ]
    let [(_rid, marks)] = UiTest.extract_rulers state
    equal (filter (is_integral . fst) (e_marks marks))
        [(0, "1"), (1, "2"), (2, "3"), (3, "4")]

    let tid = GenId.track_id_at (UiTest.bid "tscore/top") 2
    state <- return $ expect_right $ Ui.exec state $ do
        Ui.insert_event tid (Event.event 1 0 "5p")
        TScore.integrate get_ext_dur "top = \"block title\" [s r s]"
    equal (extract state)
        [ ( "top -- block title"
          , [ (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4s"), (1, 0, "5p"), (2, 0, "4s")])
            ]
          )
        ]
    equal (map (map Event.style) (e_events state)) $ map (map Style.StyleId)
        [ [1, 1, 1]
        , [1, 0, 1]
        ]

test_integrate_2_tracks = do
    let run state = Ui.exec state . TScore.integrate get_ext_dur
    let extract = UiTest.extract_blocks
    let state = expect_right $ run Ui.empty "top = [s r // g m]"
    equal (extract state)
        [ ( "top"
          , [ (">", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4s"), (1, 0, "4r")])
            , (">", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4g"), (1, 0, "4m")])
            ]
          )
        ]
    state <- return $ expect_right $ Ui.exec state $
        TScore.integrate get_ext_dur "top = [g r // g m]"
    equal (extract state)
        [ ( "top"
          , [ (">", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4g"), (1, 0, "4r")])
            , (">", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4g"), (1, 0, "4m")])
            ]
          )
        ]

test_integrate_sub_block = do
    let run state = Ui.exec state . TScore.integrate get_ext_dur
    let extract = UiTest.extract_blocks
    let state = expect_right $ run Ui.empty "top = [s [r]/]"
    equal (extract state)
        [ ("top", UiTest.note_track [(0, 1, "4s"), (1, 1, "-1c1 -- 4s")])
        , ("top-1c1", UiTest.note_track [(0, 1, "4r")])
        ]
    state <- return $ expect_right $ Ui.exec state $
        TScore.integrate get_ext_dur "top = [s [g]/ [r]/]"
    equal (extract state)
        [ ("top", UiTest.note_track
            [(0, 1, "4s"), (1, 1, "-1c1 -- 4s"), (2, 1, "-1c2 -- 4s")])
        , ("top-1c1", UiTest.note_track [(0, 1, "4g")])
        , ("top-1c2", UiTest.note_track [(0, 1, "4r")])
        ]

test_check_recursion = do
    let f = TScore.check_recursion . map (TScore.track_tokens <$>)
            . parsed_blocks
    equal (f "b1 = [a]") Nothing
    equal (f "b1 = [b2/]") Nothing
    equal (f "b1 = [b1/]") $ Just "recursive loop: b1, b1"
    equal (f "b1 = [b2/]\nb2 = [b1/]") $ Just "recursive loop: b2, b1, b2"

get_ext_dur :: TScore.GetExternalCallDuration
get_ext_dur = \_ -> (Left "not supported", [])

parsed_blocks :: Text -> [TScore.Block TScore.ParsedTrack]
parsed_blocks = expect_right . TScore.parsed_blocks

is_integral :: RealFrac a => a -> Bool
is_integral = (==0) . snd . properFraction

e_marks :: [Meter.LabeledMark] -> [(TrackTime, Meter.Label)]
e_marks = map (second Meter.m_label) . Seq.scanl_on (+) Meter.m_duration 0
