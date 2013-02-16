module Cmd.Integrate_test where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Ranges as Ranges
import Util.Test

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ResponderTest as ResponderTest

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import Types


test_block_integrate = do
    let states = mkstates "<< | reverse"
            ("s/i1", [(0, 1, "4c"), (1, 1, "4d")], [])
    res <- start states $ UiTest.insert_event 1 (1, 1, "")
    -- create a new block
    equal (e_tracks res)
        [ (UiTest.bid "b1",
            [ (">s/i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ])
        , (UiTest.bid "b2",
            [ (">s/i1", [(0, 1, ""), (1, 1, "")])
            , ("*twelve", [(0, 0, "4d"), (1, 0, "4c")])
            ])
        ]
    -- merge in changes
    res <- next res $
        UiTest.insert_event 1 (2, 1, "") >> UiTest.insert_event 2 (2, 0, "4e")
    equal (last (e_tracks res))
        (UiTest.bid "b2",
            [ (">s/i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*twelve", [(0, 0, "4e"), (1, 0, "4d"), (2, 0, "4c")])
            ])
    -- delete an event, then change the source
    -- cde -> edc ;; cdf -> f c
    res <- next res $
        UiTest.remove_event_in "b2" 1 1 >> UiTest.remove_event_in "b2" 2 1
        >> UiTest.insert_event 2 (2, 0, "4f")
    equal (last (e_tracks res))
        (UiTest.bid "b2",
            [ (">s/i1", [(0, 1, ""), (2, 1, "")])
            , ("*twelve", [(0, 0, "4f"), (2, 0, "4c")])
            ])

test_block_integrate2 = do
    let states = mkstates "<<" ("s/i1", [(0, 1, "4c"), (1, 1, "4g")], [])
        source = "b1"
        dest = "b2"
        pitch_track = fmap (snd . last) . lookup (UiTest.bid dest) . e_tracks
    res <- start states (return ())
    res <- next res $ do
        UiTest.insert_event_in dest 2 (0.5, 0, "4a")
        UiTest.insert_event_in source 2 (0.25, 0, "4d")
    res <- next res $ UiTest.insert_event_in source 2 (0.5, 0, "4e")
    equal (pitch_track res) $ Just
        [(0, 0, "4c"), (0.25, 0, "4d"), (0.5, 0, "4a"), (1, 0, "4g")]
    res <- next res $ UiTest.insert_event_in source 2 (0.75, 0, "4f")
    equal (pitch_track res) $ Just
        [(0, 0, "4c"), (0.25, 0, "4d"), (0.5, 0, "4a"), (0.75, 0, "4f"),
            (1, 0, "4g")]

test_track_integrate = do
    let states = ResponderTest.mkstates $ UiTest.note_spec
            ("s/i1 | < | reverse", [(0, 1, "4c"), (1, 1, "4d")], [])
        continue = ResponderTest.continue_until ResponderTest.is_derive_complete
        e_damage =
            fmap (Map.toList . Derive.sdamage_tracks . Cmd.perf_score_damage)
            . e_perf

    res <- start states $ return ()
    equal (e_tracks res)
        [(UiTest.default_block_id,
            [ (">s/i1 | < | reverse", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            , (">s/i1", [(0, 1, ""), (1, 1, "")])
            , ("*twelve", [(0, 0, "4d"), (1, 0, "4c")])
            ])]
    equal (e_damage res) $ Just
        [(UiTest.mk_tid n, Ranges.everything) | n <- [1..4]]
    -- Not derived yet.
    equal (e_events res) ([], [])
    res <- last <$> continue res
    equal (e_events res) ([(0, 1, "4d"), (1, 1, "4c")], [])

    res <- next res $ UiTest.insert_event 2 (0, 0, "3c")
    equal (e_tracks res)
        [(UiTest.default_block_id,
            [ (">s/i1 | < | reverse", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "3c"), (1, 0, "4d")])
            , (">s/i1", [(0, 1, ""), (1, 1, "")])
            , ("*twelve", [(0, 0, "4d"), (1, 0, "3c")])
            ])]
    equal (e_damage res) $ Just [(UiTest.mk_tid 4, Ranges.range 1 1)]
    equal (e_events res) ([(0, 1, "4d"), (1, 1, "4c")], [])
    res <- last <$> continue res
    equal (e_events res) ([(0, 1, "4d"), (1, 1, "3c")], [])

    -- TODO make a call that creates more or fewer tracks

-- multiple integrations of different tracks
-- multiple integrations of the same track

start :: ResponderTest.States -> Cmd.CmdT IO a -> IO ResponderTest.Result
start states action = last <$> until_complete states action

next :: ResponderTest.Result -> Cmd.CmdT IO a -> IO ResponderTest.Result
next = start . ResponderTest.result_states

until_complete :: ResponderTest.States -> Cmd.CmdT IO a
    -> IO [ResponderTest.Result]
until_complete = ResponderTest.respond_until ResponderTest.is_derive_complete

e_tracks :: ResponderTest.Result -> [(BlockId, [UiTest.TrackSpec])]
e_tracks = UiTest.extract_all_tracks . ResponderTest.result_ui_state

e_track_ids :: ResponderTest.Result -> [(BlockId, [TrackId])]
e_track_ids = UiTest.extract_track_ids . ResponderTest.result_ui_state

e_events :: ResponderTest.Result -> ([(RealTime, RealTime, String)], [String])
e_events = DeriveTest.extract_levents DeriveTest.e_note
    . CmdTest.e_events UiTest.default_block_id . ResponderTest.result_cmd

e_perf :: ResponderTest.Result -> Maybe Cmd.Performance
e_perf =
    CmdTest.e_performance UiTest.default_block_id . ResponderTest.result_cmd

mkstates :: String -> UiTest.NoteSpec -> ResponderTest.States
mkstates title notes = (UiTest.exec ui_state set_title, cmd_state)
    where
    (ui_state, cmd_state) = ResponderTest.mkstates (UiTest.note_spec notes)
    set_title = State.set_block_title UiTest.default_block_id title

run :: String -> [UiTest.TrackSpec] -> Cmd.CmdId a -> CmdTest.Result a
run title tracks = CmdTest.run ustate CmdTest.default_cmd_state
    where
    ustate = UiTest.exec State.empty $ do
        UiTest.mkblock_view (UiTest.default_block_name, tracks)
        State.set_block_title UiTest.default_block_id title

{-

TrackId vs. TrackNum

I should be able to use only one.  Having both around is a hassle.

Regardless, I need to know where a track is located.  I can do that with
a TrackId -> TrackNum mapping.

So:

I need a persistent ID.  Also I think I might want to share tracks between
blocks.

Use TrackId, but Ui.State doesn't let you add the same TrackId twice to one
block.  Never use TrackNum except when I specifically need location, e.g.
Create.track.  Even there, they can take TrackId.

Selection must still use TrackNum.

Both tests and Create start at 0.

-}
