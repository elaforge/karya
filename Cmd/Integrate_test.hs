module Cmd.Integrate_test where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import Util.Test

import qualified Ui.Event as Event
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
        next state action = last <$> until_complete state action
    res <- next states $ insert_event 1 (1, 1, "")
    -- create a new block
    equal (e_tracks res)
        [ (UiTest.bid "b00",
            [ (">s/i1", [(0, 1, ""), (1, 1, "")])
            , ("*twelve", [(0, 0, "4d"), (1, 0, "4c")])
            ])
        , (UiTest.bid "b1",
            [ (">s/i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ])
        ]
    -- merge in changes
    res <- next (ResponderTest.result_states res) $
        insert_event 1 (2, 1, "") >> insert_event 2 (2, 0, "4e")
    equal (head (e_tracks res))
        (UiTest.bid "b00",
            [ (">s/i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*twelve", [(0, 0, "4e"), (1, 0, "4d"), (2, 0, "4c")])
            ])
    -- delete an event, then change the source
    -- cde -> edc ;; cdf -> f c
    res <- next (ResponderTest.result_states res) $
        -- UiTest created tracks start at t01 so that I can write the IDs as
        -- strings and have the number equal the tracknum.  But Cmd.Create
        -- created tracks start at 00.
        remove_event "b00" 0 1 >> remove_event "b00" 1 1
        >> insert_event 2 (2, 0, "4f")
    equal (head (e_tracks res))
        (UiTest.bid "b00",
            [ (">s/i1", [(0, 1, ""), (2, 1, "")])
            , ("*twelve", [(0, 0, "4f"), (2, 0, "4c")])
            ])

test_track_integrate = do
    let states = ResponderTest.mkstates $ UiTest.note_spec
            ("s/i1 | < | reverse", [(0, 1, "4c"), (1, 1, "4d")], [])
        next state action = last <$> until_complete state action
        continue = ResponderTest.continue_until ResponderTest.is_derive_complete
        e_damage = fmap
            (Map.keys . Derive.sdamage_tracks . Cmd.perf_score_damage) . e_perf

    res <- next states $ return ()
    equal (e_tracks res)
        [(UiTest.default_block_id,
            [ (">s/i1 | < | reverse", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            , (">s/i1", [(0, 1, ""), (1, 1, "")])
            , ("*twelve", [(0, 0, "4d"), (1, 0, "4c")])
            ])]
    equal (e_damage res) $ Just [UiTest.mk_tid 0, UiTest.mk_tid 3]
        -- The new tracks are 0, 3, due to UiTest starting at 1 and and
        -- Cmd.Create starting at 0.  TODO fix this by making both start at 0.
        -- Or maybe make the ruler be t00, that way Cmd.Create will skip it.
    Pretty.pprint ((CmdTest.e_events UiTest.default_block_id . ResponderTest.result_cmd) res)
    -- Not derived yet.
    equal (e_events res) ([], [])
    res <- last <$> continue res
    equal (e_events res) ([(0, 1, "4d"), (1, 1, "4c")], [])

    res <- next (ResponderTest.result_states res) $ insert_event 2 (0, 0, "3c")
    equal (e_tracks res)
        [(UiTest.default_block_id,
            [ (">s/i1 | < | reverse", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "3c"), (1, 0, "4d")])
            , (">s/i1", [(0, 1, ""), (1, 1, "")])
            , ("*twelve", [(0, 0, "4d"), (1, 0, "3c")])
            ])]
    equal (e_events res) ([(0, 1, "4d"), (1, 1, "4c")], [])
    res <- last <$> continue res
    equal (e_events res) ([(0, 1, "4d"), (1, 1, "3c")], [])

    -- TODO make a call that creates more or fewer tracks

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

until_complete :: ResponderTest.States -> Cmd.CmdT IO a
    -> IO [ResponderTest.Result]
until_complete = ResponderTest.respond_until ResponderTest.is_derive_complete

e_tracks :: ResponderTest.Result -> [(BlockId, [UiTest.TrackSpec])]
e_tracks = UiTest.extract_all_tracks . ResponderTest.result_ui_state

e_events :: ResponderTest.Result -> ([(RealTime, RealTime, String)], [String])
e_events = DeriveTest.extract_levents DeriveTest.e_note2
    . CmdTest.e_events UiTest.default_block_id . ResponderTest.result_cmd

e_perf :: ResponderTest.Result -> Maybe Cmd.Performance
e_perf =
    CmdTest.e_performance UiTest.default_block_id . ResponderTest.result_cmd

insert_event :: (State.M m) => TrackNum -> (ScoreTime, ScoreTime, String)
    -> m ()
insert_event tracknum (pos, dur, text) =
    State.insert_event (UiTest.mk_tid tracknum) pos (Event.event text dur)

remove_event :: (State.M m) => String -> TrackNum -> ScoreTime -> m ()
remove_event name tracknum =
    State.remove_event (UiTest.mk_tid_name name tracknum)

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
