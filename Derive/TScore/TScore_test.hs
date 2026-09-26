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
    let f = parsedScore
    rightEqual (f "top = \"block title\" [s r g]")
        [ ( "top -- block title"
          , [ (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4s"), (1, 0, "4r"), (2, 0, "4g")])
            ]
          )
        ]
    rightEqual (f "top = %default-call [>\" | rh\" na din >lh _ thom]")
        [ ( "top"
          , [ (">lh", [(1, 1, "thom")])
            , ("> | rh", [(0, 1, "na"), (1, 1, "din")])
            ]
          )
        ]
    -- sub-blocks
    rightEqual (f "top = [s [r [g m]/]/]")
        [ ("top", UiTest.noteTrack1 ["4s", "-t1c1 --"])
        , ("top-t1c1", UiTest.noteTrack1 ["4r", "-t1c1 --"])
        , ("top-t1c1-t1c1", UiTest.noteTrack1 ["4g", "4m"])
        ]
    rightEqual (f "top = [s +pizz|[r g]/]")
        -- Test TScore.pipe_tweak, which emits "+pizz |" instead of "+pizz|".
        [ ("top", UiTest.noteTrack1 ["4s", "+pizz | -t1c1 --"])
        , ("top-t1c1", UiTest.noteTrack [(0, 1, "4r"), (1, 1, "4g")])
        ]
    rightEqual (f "top = [ x[s][r]/ ]")
        [ ("top", [(">", [(0, 1, "x -t1c1a -t1c1b")])])
        , ("top-t1c1a", UiTest.noteTrack1 ["4s"])
        , ("top-t1c1b", UiTest.noteTrack1 ["4r"])
        ]
    rightEqual (f "top = [ [s r]/ > [g m]/ ]")
        [ ("top", [(">", [(0, 1, "-t1c1")]), (">", [(0, 1, "-t2c1")])])
        , ("top-t1c1", UiTest.noteTrack1 ["4g", "4m"])
        , ("top-t2c1", UiTest.noteTrack1 ["4s", "4r"])
        ]
    -- Trailing rests are treated properly.
    rightEqual (f "top = [ [s _]/0 ]")
        [ ("top", [(">", [(0, 2, "-t1c1")])])
        , ("top-t1c1", UiTest.noteTrack1 ["4s"])
        ]

test_skeleton :: Test
test_skeleton = do
    let f = fmap (Map.elems . UiTest.extractSkeletons) . parseScore
    rightEqual (f "top = [s r g]") [[(1, 2)]]
    rightEqual (f "top = [>i1 s r g >i2 m p d]") [[(1, 2), (3, 4)]]

test_copy_from :: Test
test_copy_from = do
    let f = fmap (lookup "b1") . parsedScore
    let track = Just . UiTest.noteTrack1
        tracks = Just . concatMap UiTest.noteTrack1
    leftLike (f "%f=x\nb1 = [s]") "can't use at global scope"
    leftLike (f "b1 = %f=b2 [s r]") "block not found"
    leftLike (f "b1 = [> %f=b1 s r]") "not a tracknum"
    leftLike (f "b1 = [> %f=1 s r]") "can't copy from the same track"
    leftLike (f "b1 = [>! %f=3 s r > g m]") "doesn't have track 3"
    leftLike (f "b1 = %f=b2 [> %f=2 s r]\nb2 = [g m]") "doesn't have track 2"
    leftLike (f "b1 = [>! %f=2 ^ ^ > s ]") "no notes to copy in range"

    -- from other block
    rightEqual (f "b1 = %f=b2 [s r]\nb2 = [g m]") (track ["4s", "4r"])
    rightEqual (f "b1 = %f=b2 [s ^]\nb2 = [g m]") (track ["4s", "4m"])
    rightEqual (f "b1 = %f=b2 [^ r]\nb2 = [g m]") (track ["4g", "4r"])
    rightEqual (f "b1 = %f=b2 [^2:1]\nb2 = [g m]") (track ["4g", "4m"])
    rightEqual (f "b1 = %f=b2 [> %f=2 s ^]\nb2 = [g m > p d]")
        (track ["4s", "4d"])

    -- from same block
    rightEqual (f "b1 = [>! %f=2 s r > g m]")
        (tracks [["4g", "4m"], ["4s", "4r"]])
    rightEqual (f "b1 = [>! %f=2 ^ r > g m]")
        (tracks [["4g", "4m"], ["4g", "4r"]])

    -- Two steps of indirection.
    leftLike (f "b1 = [> %f=2 ^ > %f=3 ^ > s]") "recursive %f"
    leftLike (f "b1 = [> %f=2 ^ r > %f=1 ^ m]") "recursive %f"

test_assert_coincident :: Test
test_assert_coincident = do
    let f = fmap (const ()) . parseScore
    leftLike (f "top = [s r > g ; m]") "got unexpected assert"
    leftLike (f "top = [s ; r > g m]") "expected assert here"
    rightEqual (f "top = [s ; r > g ; m]") ()
    leftLike (f "top = [s ; r ; > g ; m p]") "expected assert"
    rightEqual (f "top = [s ; r ; > g ; m ;]") ()

    -- Don't enforce for an empty track, or if it's past the end of the notes.
    rightEqual (f "top = [>! s r ; g >]") ()
    rightEqual (f "top = [>! s r ; g > s]") ()

test_wrapped_tracks :: Test
test_wrapped_tracks = do
    let f = parsedScore
    rightEqual (f "top = [s r > g m] [p d > n s]")
        [ ("top", concat
            [ UiTest.noteTrack1 ["4g", "4m", "4n", "5s"]
            , UiTest.noteTrack1 ["4s", "4r", "4p", "4d"]
            ])
        ]
    rightEqual (f "top = [>i1 s >i2 r] [>i1 g >i2 m]")
        [ ("top", concat
            [ UiTest.instNoteTrack1 "i2" ["4r", "4m"]
            , UiTest.instNoteTrack1 "i1" ["4s", "4g"]
            ])
        ]
    leftLike (f "top = [>i1 s >i2 r] [>i1 g >i1 m]")
        "wrapped track titles must match"
    leftLike (f "top = [>i1 s >i2 r] [>i1 g]")
        "wrapped track titles must match"

    leftLike (f "top = [ s r > g] [m p > d n]") "expected assert here"

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
    let f = parsedScore
    let bBlock = ("b", UiTest.noteTrack [(0, 1, "4s"), (1, 1, "4r")])
    rightEqual (f "a = [b/]\nb = [s r]")
        [ ("a", [(">", [(0, 1, "b")])])
        , bBlock
        ]
    rightEqual (f "a = [b/0]\nb = [s r]")
        [ ("a", [(">", [(0, 2, "b")])])
        , bBlock
        ]
    -- CallDuration is carried like other durs.
    rightEqual (f "a = %default-call [b/0 b]\nb = [s r]")
        [ ("a", [(">", [(0, 2, "b"), (2, 2, "b")])])
        , bBlock
        ]
    -- The default dur carries past CallDuration.
    rightEqual (f "a = [b/0 s]\nb = [s r]")
        [ ("a",
            [ (">", [(0, 2, "b"), (2, 1, "")])
            , ("*", [(2, 0, "4s")])
            ])
        , bBlock
        ]
    rightEqual (f "a = [b/0 _ b/]\nb = [s r]")
        [ ("a", [(">", [(0, 2, "b"), (3, 2, "b")])])
        , bBlock
        ]
    -- Works for sub-blocks.
    rightEqual (f "a = [[s r]/0 g]")
        [ ("a", UiTest.noteTrack [(0, 2, "-t1c1 --"), (2, 1, "4g")])
        , ("a-t1c1", UiTest.noteTrack1 ["4s", "4r"])
        ]

test_ext_call_duration :: Test
test_ext_call_duration = do
    let f blocks source = extract $
            CmdTest.runBlocks blocks (TScore.cmdIntegrate source)
        extract = fmap (Lists.sortOn fst) . CmdTest.traceLogs
            . CmdTest.extractUiState UiTest.extractBlocks
    let blocks = [("top=ruler", UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])]
        top = ("top", UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])
    -- name collision
    leftLike (f blocks "top = [s1]") "block to integrate already exists"

    -- It uses the root block's namespace, not tscore.
    rightEqual (f blocks "a = [top/0 s1]")
        [ ("a", UiTest.noteTrack [(0, 2, "top --"), (2, 1, "4s")])
        , top
        ]

    -- Use block title for context.
    rightEqual
        (f blocks "a = \"import india.mridangam | dur=1\" [> \"8n\"/0 s1]")
        [ ("a -- import india.mridangam | dur=1",
            [ (">", [(0, 8, "8n"), (8, 1, "")])
            , ("*", [(8, 0, "4s")])
            ])
        , top
        ]
    -- Use the track title too.
    rightEqual
        (f blocks "a = \"import india.mridangam\" [>\" | dur=1\" \"8n\"/0 s1]")
        [ ("a -- import india.mridangam",
            [ ("> | dur=1", [(0, 8, "8n"), (8, 1, "")])
            , ("*", [(8, 0, "4s")])
            ])
        , top
        ]

eEvents :: Ui.State -> [[Event.Event]]
eEvents = map (Events.ascending . Track.trackEvents) . Map.elems
    . Ui.stateTracks

test_integrate :: Test
test_integrate = do
    let run state = Ui.exec state . integrate
    let extract = UiTest.extractBlocks
    let state = expectRight $ run Ui.empty "top = \"block title\" [s r g]"

    equal (extract state)
        [ ("top -- block title",
            UiTest.noteTrack [(0, 1, "4s"), (1, 1, "4r"), (2, 1, "4g")])
        ]
    -- Make sure style and ruler are as expected.
    equal (map (map Event.style) (eEvents state)) $ map (map Style.StyleId)
        [ [1, 1, 1]
        , [1, 1, 1]
        ]
    let eMarks = map (second (filter (isIntegral . fst))) . UiTest.eMeters
    equal (eMarks state)
        [("top", [(0, (0, "1")), (1, (1, "2")), (2, (1, "3")), (3, (0, "4"))])]

    state <- pure $ expectRight $ Ui.exec state $ do
        Ui.insertEvent (tid "top" 2) (Event.event 1 0 "5p")
        integrate "top = \"block title\" [s r s]"
    equal (extract state)
        [ ( "top -- block title"
          , [ (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4s"), (1, 0, "5p"), (2, 0, "4s")])
            ]
          )
        ]
    equal (map (map Event.style) (eEvents state)) $ map (map Style.StyleId)
        [ [1, 1, 1]
        , [1, 0, 1]
        ]

test_integrate_block_duration :: Test
test_integrate_block_duration = do
    let run state = Ui.exec state . integrate
        extract ui = Ui.eval ui $ mapM Ui.blockEnd $ Map.keys $
            Ui.stateBlocks ui
    let state = expectRight $ run Ui.empty "b = [s r g]"
    rightEqual (extract state) [3]
    state <- pure $ expectRight $ Ui.exec state $ integrate "b = [s r g m]"
    rightEqual (extract state) [4]
    state <- pure $ expectRight $ Ui.exec state $ integrate "b = [s r]"
    rightEqual (extract state) [2]

test_integrate_misc :: Test
test_integrate_misc = do
    let run state = Ui.exec state . integrate
    let extract = UiTest.extractBlocks
    let state = expectRight $ run Ui.empty "top = [s]"
    equal (extract state) [("top", UiTest.noteTrack [(0, 1, "4s")])]
    state <- pure $ expectRight $ Ui.exec state $
        integrate "top = \"block title\" [s]"
    equal (extract state)
        [("top -- block title", UiTest.noteTrack [(0, 1, "4s")])]

test_integrate_2_tracks :: Test
test_integrate_2_tracks = do
    let run state = Ui.exec state . integrate
    let extract = UiTest.extractBlocks
    let state = expectRight $ run Ui.empty "top = [s r > g m]"
    equal (extract state)
        [ ( "top"
          , [ (">", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4g"), (1, 0, "4m")])
            , (">", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4s"), (1, 0, "4r")])
            ]
          )
        ]
    state <- pure $ expectRight $ Ui.exec state $
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
    let extract = UiTest.extractBlocks

    -- The rename is detected, even if you changed the track title.
    let state = expectRight $ run Ui.empty "b1 = [>i1 s]"
    equal (extract state) [("b1", UiTest.instNoteTrack1 "i1" ["4s"])]
    state <- pure $ expectRight $ Ui.exec state $ integrate "b2 = [>i2 s]"
    equal (extract state) [("b2", UiTest.instNoteTrack1 "i2" ["4s"])]

    -- But if you changed notes at the same time, it can't find the rename.
    let state = expectRight $ run Ui.empty "b1 = [>i1 s]"
    state <- pure $ expectRight $ Ui.exec state $ integrate "b2 = [>i2 r]"
    equal (extract state)
        [ ("b1", UiTest.instNoteTrack1 "i1" ["4s"])
        , ("b2", UiTest.instNoteTrack1 "i2" ["4r"])
        ]

test_integrate_move_track :: Test
test_integrate_move_track = do
    let extract = UiTest.extractBlocks
    state <- either (errorIO . pretty) pure $ Ui.exec Ui.empty $ do
        integrate "b1 = [>i1 s >i2 r]"
        forM_ [1, 2] $ \i ->
            Ui.modifyEvents (tid "b1" i) (Events.move 2)
        forM_ [3, 4] $ \i ->
            Ui.modifyEvents (tid "b1" i) (Events.move 1)
    let i1 = UiTest.instNoteTrack "i1" [(1, 1, "4s")]
        i2 = UiTest.instNoteTrack "i2" [(2, 1, "4r")]
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
    let extract = UiTest.extractBlocks
    let state = expectRight $ run Ui.empty "top = [s [r]/]"
    equal (extract state)
        [ ("top", UiTest.noteTrack [(0, 1, "4s"), (1, 1, "-t1c1 --")])
        , ("top-t1c1", UiTest.noteTrack [(0, 1, "4r")])
        ]
    state <- pure $ expectRight $ Ui.exec state $
        integrate "top = [s [g]/ [r]/]"
    equal (extract state)
        [ ("top", UiTest.noteTrack
            [(0, 1, "4s"), (1, 1, "-t1c1 --"), (2, 1, "-t1c2 --")])
        , ("top-t1c1", UiTest.noteTrack [(0, 1, "4g")])
        , ("top-t1c2", UiTest.noteTrack [(0, 1, "4r")])
        ]

test_check_unique_track_keys :: Test
test_check_unique_track_keys = do
    let f = fmap (const ()) . parseScore
    rightEqual (f "b1 = [>!i1 s >@i1 r]") ()
    rightEqual (f "b1 = [>i1 s >i2 r]") ()
    leftLike (f "b1 = [>i1 s >i1 r]") "non-unique track key"
    leftLike (f "b1 = [>!i1 s >!i1 r]") "non-unique track key"

test_check_recursion :: Test
test_check_recursion = do
    let f = TScore.checkRecursion . map (TScore.trackTokens <$>)
            . parsedBlocks
    equal (f "b1 = [a]") Nothing
    equal (f "b1 = [b2/]") Nothing
    equal (f "b1 = [b1/]") $ Just "recursive loop: b1, b1"
    equal (f "b1 = [b2/]\nb2 = [b1/]") $ Just "recursive loop: b2, b1, b2"

integrate :: Ui.M m => Text -> m [BlockId]
integrate = TScore.integrate makeExtDur

-- | Tests exercise tscore, not the deriver, so don't give them a derive
-- environment.  'TScore.standalone_duration' is the one that does.
makeExtDur :: TScore.MakeExternalCallDuration
makeExtDur = const $ Right TScore.noExternalDuration

parseScore :: Text -> Either Text Ui.State
parseScore = TScore.scoreToUi makeExtDur

parsedScore :: Text -> Either Text [UiTest.BlockSpec]
parsedScore = fmap UiTest.extractBlocks . parseScore

parsedBlocks :: Text -> [TScore.Block TScore.ParsedTrack]
parsedBlocks = fst . expectRight . TScore.parseBlocks

isIntegral :: RealFrac a => a -> Bool
isIntegral = (==0) . snd . properFraction

tid :: Text -> TrackNum -> TrackId
tid block = GenId.trackIdAt (UiTest.bid ("untitled/" <> block))
