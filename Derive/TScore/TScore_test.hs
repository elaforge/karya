-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.TScore.TScore_test where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Cmd.CmdTest as CmdTest
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


test_parse_score :: Test
test_parse_score = do
    let f = parsed_score
    right_equal (f "top = \"block title\" [s r g]")
        [ ( "top -- block title"
          , [ (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4s"), (1, 0, "4r"), (2, 0, "4g")])
            ]
          )
        ]
    right_equal (f "top = %default-call [>\" | rh\" na din >lh _ thom]")
        [ ( "top"
          , [ (">lh", [(1, 1, "thom")])
            , ("> | rh", [(0, 1, "na"), (1, 1, "din")])
            ]
          )
        ]
    -- sub-blocks
    right_equal (f "top = [s [r [g m]/]/]")
        [ ("top", UiTest.note_track1 ["4s", "-t1c1 --"])
        , ("top-t1c1", UiTest.note_track1 ["4r", "-t1c1 --"])
        , ("top-t1c1-t1c1", UiTest.note_track1 ["4g", "4m"])
        ]
    right_equal (f "top = [s +pizz|[r g]/]")
        -- Test TScore.pipe_tweak, which emits "+pizz |" instead of "+pizz|".
        [ ("top", UiTest.note_track1 ["4s", "+pizz | -t1c1 --"])
        , ("top-t1c1", UiTest.note_track [(0, 1, "4r"), (1, 1, "4g")])
        ]
    right_equal (f "top = [ x[s][r]/ ]")
        [ ("top", [(">", [(0, 1, "x -t1c1a -t1c1b")])])
        , ("top-t1c1a", UiTest.note_track1 ["4s"])
        , ("top-t1c1b", UiTest.note_track1 ["4r"])
        ]
    right_equal (f "top = [ [s r]/ > [g m]/ ]")
        [ ("top", [(">", [(0, 1, "-t1c1")]), (">", [(0, 1, "-t2c1")])])
        , ("top-t1c1", UiTest.note_track1 ["4g", "4m"])
        , ("top-t2c1", UiTest.note_track1 ["4s", "4r"])
        ]
    -- Trailing rests are treated properly.
    right_equal (f "top = [ [s _]/0 ]")
        [ ("top", [(">", [(0, 2, "-t1c1")])])
        , ("top-t1c1", UiTest.note_track1 ["4s"])
        ]

test_skeleton :: Test
test_skeleton = do
    let f = fmap (Map.elems . UiTest.extract_skeletons . fst)
            . TScore.parse_score
    right_equal (f "top = [s r g]") [[(1, 2)]]
    right_equal (f "top = [>i1 s r g >i2 m p d]") [[(1, 2), (3, 4)]]

test_copy_from :: Test
test_copy_from = do
    let f = fmap (lookup "b1") . parsed_score
    let track = Just . UiTest.note_track1
        tracks = Just . concatMap UiTest.note_track1
    left_like (f "%f=x\nb1 = [s]") "can't use at global scope"
    left_like (f "b1 = %f=b2 [s r]") "block not found"
    left_like (f "b1 = [> %f=b1 s r]") "not a tracknum"
    left_like (f "b1 = [> %f=1 s r]") "can't copy from the same track"
    left_like (f "b1 = [>! %f=3 s r > g m]") "doesn't have track 3"
    left_like (f "b1 = %f=b2 [> %f=2 s r]\nb2 = [g m]") "doesn't have track 2"
    left_like (f "b1 = [>! %f=2 ^ ^ > s ]") "no notes to copy in range"

    -- from other block
    right_equal (f "b1 = %f=b2 [s r]\nb2 = [g m]") (track ["4s", "4r"])
    right_equal (f "b1 = %f=b2 [s ^]\nb2 = [g m]") (track ["4s", "4m"])
    right_equal (f "b1 = %f=b2 [^ r]\nb2 = [g m]") (track ["4g", "4r"])
    right_equal (f "b1 = %f=b2 [^2:1]\nb2 = [g m]") (track ["4g", "4m"])
    right_equal (f "b1 = %f=b2 [> %f=2 s ^]\nb2 = [g m > p d]")
        (track ["4s", "4d"])

    -- from same block
    right_equal (f "b1 = [>! %f=2 s r > g m]")
        (tracks [["4g", "4m"], ["4s", "4r"]])
    right_equal (f "b1 = [>! %f=2 ^ r > g m]")
        (tracks [["4g", "4m"], ["4g", "4r"]])

    -- Two steps of indirection.
    left_like (f "b1 = [> %f=2 ^ > %f=3 ^ > s]") "recursive %f"
    left_like (f "b1 = [> %f=2 ^ r > %f=1 ^ m]") "recursive %f"

test_assert_coincident :: Test
test_assert_coincident = do
    let f = fmap (const ()) . TScore.parse_score
    left_like (f "top = [s r > g ; m]") "got unexpected assert"
    left_like (f "top = [s ; r > g m]") "expected assert here"
    right_equal (f "top = [s ; r > g ; m]") ()
    left_like (f "top = [s ; r ; > g ; m p]") "expected assert"
    right_equal (f "top = [s ; r ; > g ; m ;]") ()

    -- Don't enforce for an empty track, or if it's past the end of the notes.
    right_equal (f "top = [>! s r ; g >]") ()
    right_equal (f "top = [>! s r ; g > s]") ()

test_wrapped_tracks :: Test
test_wrapped_tracks = do
    let f = parsed_score
    right_equal (f "top = [s r > g m] [p d > n s]")
        [ ("top", concat
            [ UiTest.note_track1 ["4g", "4m", "4n", "5s"]
            , UiTest.note_track1 ["4s", "4r", "4p", "4d"]
            ])
        ]
    right_equal (f "top = [>i1 s >i2 r] [>i1 g >i2 m]")
        [ ("top", concat
            [ UiTest.inst_note_track1 "i2" ["4r", "4m"]
            , UiTest.inst_note_track1 "i1" ["4s", "4g"]
            ])
        ]
    left_like (f "top = [>i1 s >i2 r] [>i1 g >i1 m]")
        "wrapped track titles must match"
    left_like (f "top = [>i1 s >i2 r] [>i1 g]")
        "wrapped track titles must match"

    left_like (f "top = [ s r > g] [m p > d n]") "expected assert here"

-- test_ui_skeleton :: Test
-- test_ui_skeleton = do
--     let f = Skeleton.flatten . TScore.ui_skeleton
--         nt n = TScore.NTrack t "" (replicate (n-1) t) 0
--             where t = TScore.Track "" mempty
--     equal (f [nt 2]) [(1, 2)]
--     equal (f [nt 2, nt 2]) [(1, 2), (3, 4)]
--     equal (f [nt 2, nt 1, nt 2]) [(1, 2), (4, 5)]

test_call_duration :: Test
test_call_duration = do
    let f = parsed_score
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
    right_equal (f "a = %default-call [b/0 b]\nb = [s r]")
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
        [ ("a", UiTest.note_track [(0, 2, "-t1c1 --"), (2, 1, "4g")])
        , ("a-t1c1", UiTest.note_track1 ["4s", "4r"])
        ]

test_ext_call_duration :: Test
test_ext_call_duration = do
    let f blocks source = extract $
            CmdTest.run_blocks blocks (TScore.cmd_integrate source)
        extract = fmap (Lists.sortOn fst) . CmdTest.trace_logs
            . CmdTest.extract_ui_state UiTest.extract_blocks
    let blocks = [("top=ruler", UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])]
        top = ("top", UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
    -- name collision
    left_like (f blocks "top = [s1]") "block to integrate already exists"

    -- It uses the root block's namespace, not tscore.
    right_equal (f blocks "a = [top/0 s1]")
        [ ("a", UiTest.note_track [(0, 2, "top --"), (2, 1, "4s")])
        , top
        ]

    -- Use block title for context.
    right_equal
        (f blocks "a = \"import india.mridangam | dur=1\" [> \"8n\"/0 s1]")
        [ ("a -- import india.mridangam | dur=1",
            [ (">", [(0, 8, "8n"), (8, 1, "")])
            , ("*", [(8, 0, "4s")])
            ])
        , top
        ]
    -- Use the track title too.
    right_equal
        (f blocks "a = \"import india.mridangam\" [>\" | dur=1\" \"8n\"/0 s1]")
        [ ("a -- import india.mridangam",
            [ ("> | dur=1", [(0, 8, "8n"), (8, 1, "")])
            , ("*", [(8, 0, "4s")])
            ])
        , top
        ]

e_events :: Ui.State -> [[Event.Event]]
e_events = map (Events.ascending . Track.track_events) . Map.elems
    . Ui.state_tracks

test_integrate :: Test
test_integrate = do
    let run state = Ui.exec state . integrate
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
    let e_marks = map (second (filter (is_integral . fst))) . UiTest.e_meters
    equal (e_marks state)
        [("top", [(0, (0, "1")), (1, (1, "2")), (2, (1, "3")), (3, (0, "4"))])]

    state <- return $ expect_right $ Ui.exec state $ do
        Ui.insert_event (tid "top" 2) (Event.event 1 0 "5p")
        integrate "top = \"block title\" [s r s]"
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

test_integrate_block_duration :: Test
test_integrate_block_duration = do
    let run state = Ui.exec state . integrate
        extract ui = Ui.eval ui $ mapM Ui.block_end $ Map.keys $
            Ui.state_blocks ui
    let state = expect_right $ run Ui.empty "b = [s r g]"
    right_equal (extract state) [3]
    state <- return $ expect_right $ Ui.exec state $ integrate "b = [s r g m]"
    right_equal (extract state) [4]
    state <- return $ expect_right $ Ui.exec state $ integrate "b = [s r]"
    right_equal (extract state) [2]

test_integrate_misc :: Test
test_integrate_misc = do
    let run state = Ui.exec state . integrate
    let extract = UiTest.extract_blocks
    let state = expect_right $ run Ui.empty "top = [s]"
    equal (extract state) [("top", UiTest.note_track [(0, 1, "4s")])]
    state <- return $ expect_right $ Ui.exec state $
        integrate "top = \"block title\" [s]"
    equal (extract state)
        [("top -- block title", UiTest.note_track [(0, 1, "4s")])]

test_integrate_2_tracks :: Test
test_integrate_2_tracks = do
    let run state = Ui.exec state . integrate
    let extract = UiTest.extract_blocks
    let state = expect_right $ run Ui.empty "top = [s r > g m]"
    equal (extract state)
        [ ( "top"
          , [ (">", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4g"), (1, 0, "4m")])
            , (">", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4s"), (1, 0, "4r")])
            ]
          )
        ]
    state <- return $ expect_right $ Ui.exec state $
        integrate "top = [g r > g m]"
    equal (extract state)
        [ ( "top"
          , [ (">", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4g"), (1, 0, "4m")])
            , (">", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4g"), (1, 0, "4r")])
            ]
          )
        ]

test_integrate_rename :: Test
test_integrate_rename = do
    let run state = Ui.exec state . integrate
    let extract = UiTest.extract_blocks

    -- The rename is detected, even if you changed the track title.
    let state = expect_right $ run Ui.empty "b1 = [>i1 s]"
    equal (extract state) [("b1", UiTest.inst_note_track1 "i1" ["4s"])]
    state <- return $ expect_right $ Ui.exec state $ integrate "b2 = [>i2 s]"
    equal (extract state) [("b2", UiTest.inst_note_track1 "i2" ["4s"])]

    -- But if you changed notes at the same time, it can't find the rename.
    let state = expect_right $ run Ui.empty "b1 = [>i1 s]"
    state <- return $ expect_right $ Ui.exec state $ integrate "b2 = [>i2 r]"
    equal (extract state)
        [ ("b1", UiTest.inst_note_track1 "i1" ["4s"])
        , ("b2", UiTest.inst_note_track1 "i2" ["4r"])
        ]

test_integrate_move_track :: Test
test_integrate_move_track = do
    let extract = UiTest.extract_blocks
    state <- either (errorIO . pretty) return $ Ui.exec Ui.empty $ do
        integrate "b1 = [>i1 s >i2 r]"
        forM_ [1, 2] $ \i ->
            Ui.modify_events (tid "b1" i) (Events.move 2)
        forM_ [3, 4] $ \i ->
            Ui.modify_events (tid "b1" i) (Events.move 1)
    let i1 = UiTest.inst_note_track "i1" [(1, 1, "4s")]
        i2 = UiTest.inst_note_track "i2" [(2, 1, "4r")]
    equal (extract state) [("b1", i2 ++ i1)]

    -- let run = extract . expect_right . Ui.exec state . integrate
    -- -- >i1 was deleted, >i2 keeps its edits.
    -- equal (run "b1 = [>i2 r]") [("b1", i2)]
    --
    -- -- >i2 was deleted, >i1 keeps its edits.
    -- equal (run "b1 = [>i1 s]") [("b1", i1)]
    --
    -- -- They traded places, both should keep edits.
    -- equal (run "b1 = [>i2 r >i1 s]") [("b1", i1 ++ i2)]

test_integrate_sub_block :: Test
test_integrate_sub_block = do
    let run state = Ui.exec state . integrate
    let extract = UiTest.extract_blocks
    let state = expect_right $ run Ui.empty "top = [s [r]/]"
    equal (extract state)
        [ ("top", UiTest.note_track [(0, 1, "4s"), (1, 1, "-t1c1 --")])
        , ("top-t1c1", UiTest.note_track [(0, 1, "4r")])
        ]
    state <- return $ expect_right $ Ui.exec state $
        integrate "top = [s [g]/ [r]/]"
    equal (extract state)
        [ ("top", UiTest.note_track
            [(0, 1, "4s"), (1, 1, "-t1c1 --"), (2, 1, "-t1c2 --")])
        , ("top-t1c1", UiTest.note_track [(0, 1, "4g")])
        , ("top-t1c2", UiTest.note_track [(0, 1, "4r")])
        ]

test_check_unique_track_keys :: Test
test_check_unique_track_keys = do
    let f = fmap (const ()) . TScore.parse_score
    right_equal (f "b1 = [>!i1 s >@i1 r]") ()
    right_equal (f "b1 = [>i1 s >i2 r]") ()
    left_like (f "b1 = [>i1 s >i1 r]") "non-unique track key"
    left_like (f "b1 = [>!i1 s >!i1 r]") "non-unique track key"

test_check_recursion :: Test
test_check_recursion = do
    let f = TScore.check_recursion . map (TScore.track_tokens <$>)
            . parsed_blocks
    equal (f "b1 = [a]") Nothing
    equal (f "b1 = [b2/]") Nothing
    equal (f "b1 = [b1/]") $ Just "recursive loop: b1, b1"
    equal (f "b1 = [b2/]\nb2 = [b1/]") $ Just "recursive loop: b2, b1, b2"

integrate :: Ui.M m => Text -> m [BlockId]
integrate = TScore.integrate get_ext_dur

get_ext_dur :: TScore.GetExternalCallDuration
get_ext_dur = \_ _ -> (Left "external call dur not supported", [])

parsed_score :: Text -> Either Text [UiTest.BlockSpec]
parsed_score = fmap (UiTest.extract_blocks . fst) . TScore.parse_score

parsed_blocks :: Text -> [TScore.Block TScore.ParsedTrack]
parsed_blocks = fst . expect_right . TScore.parse_blocks

is_integral :: RealFrac a => a -> Bool
is_integral = (==0) . snd . properFraction

tid :: Text -> TrackNum -> TrackId
tid block = GenId.track_id_at (UiTest.bid ("untitled/" <> block))
