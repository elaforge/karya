-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Integrate_test where
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Log as Log
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Create as Create
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Integrate as Integrate
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.ResponderTest as ResponderTest

import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Stack as Stack

import qualified Instrument.Common as Common
import qualified Perform.Midi.Patch as Patch
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


-- * derive integration

test_block_integrate :: Test
test_block_integrate = do
    let states = mkstates "<< | reverse"
            ("i1", [(0, 1, "4c"), (1, 1, "4d")], [])
    res <- start states $ UiTest.insert_event 1 (1, 1, "")
    -- create a new block, integrate b1 to b2
    equal (e_tracks res)
        [ ("b1 -- << | reverse",
            [ (">i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ])
        , ("b2",
            [ (">i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4d"), (1, 0, "4c")])
            ])
        ]
    -- change b1, reversed changes are merged into b2
    res <- next res $ do
        UiTest.insert_event 1 (2, 1, "")
        UiTest.insert_event 2 (2, 0, "4e")
    equal (last (e_tracks res))
        ("b2",
            [ (">i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4e"), (1, 0, "4d"), (2, 0, "4c")])
            ])
    -- Delete an event in b2 and change b1.
    -- b1: 'cde' -> reverse to 'edc', remove 'd' so 'e c'
    -- b2: replace 'c' with 'f', so 'e f'
    reses <- nexts res $ do
        UiTest.remove_event_in "b2" 1 1
        UiTest.remove_event_in "b2" 2 1
        UiTest.insert_event 2 (2, 0, "4f")
    equal (last (e_tracks (last reses)))
        ("b2",
            [ (">i1", [(0, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4f"), (2, 0, "4c")])
            ])

test_block_integrate2 :: Test
test_block_integrate2 = do
    let states = mkstates "<<" ("i1", [(0, 1, "4c"), (1, 1, "4g")], [])
        source = "b1"
        dest = "b2"
        pitch_track = fmap (snd . last) . lookup dest . e_tracks
    res <- start states (return ())
    res <- next res $ do
        UiTest.insert_event_in dest 2 (0.5, 0, "4a")
        UiTest.insert_event_in source 2 (0.25, 0, "4d")
    res <- next res $ UiTest.insert_event_in source 2 (0.5, 0, "4e")
    -- 4e in src is replaced by overidden by 4a in dest
    equal (pitch_track res) $ Just
        [(0, 0, "4c"), (0.25, 0, "4d"), (0.5, 0, "4a"), (1, 0, "4g")]
    res <- next res $ UiTest.insert_event_in source 2 (0.75, 0, "4f")
    equal (pitch_track res) $ Just
        [ (0, 0, "4c"), (0.25, 0, "4d"), (0.5, 0, "4a"), (0.75, 0, "4f")
        , (1, 0, "4g")
        ]

test_block_integrate_call_map :: Test
test_block_integrate_call_map = do
    let cmd_state = CmdTest.mk_cmd_state $ UiTest.make_db1 $
            MidiInst.attribute_map #= Patch.single_keyswitches
                [(Attrs.attr "attr2", 42)] $
            MidiInst.common#Common.call_map #= Map.fromList
                [ (Attrs.attr "attr1", "one")
                ] $
            MidiInst.make_patch $ Patch.patch (-2, 2) "1"
    let states = second (const cmd_state) $ mkstates "<<"
            ( "i1"
            , [ (0, 1, "+attr1 -- 4c"), (1, 1, "+attr2 -- 4d")
              , (2, 1, "+attr3 -- 4e")
              ]
            , []
            )
    res <- start states $ return ()
    equal (e_tracks res)
        [ ( "b1 -- <<"
          , UiTest.inst_note_track1 "i1"
            ["+attr1 -- 4c", "+attr2 -- 4d", "+attr3 -- 4e"]
          )
        , ( "b2"
          , UiTest.inst_note_track1 "i1" ["one -- 4c", "+attr2 -- 4d", "4e"]
          )
        ]

test_track_integrate :: Test
test_track_integrate = do
    let states = ResponderTest.mkstates $ UiTest.note_spec
            ("i1", [(0, 1, "4c"), (1, 1, "4d")], [])
        has_integrated = fmap (not . null) . e_integrated
    -- The first track integrate causes two derives.
    res <- start states $ Ui.set_track_title (UiTest.mk_tid 1) ">i1 | <"
    equal (e_events res) []
    equal (has_integrated res) (Just True)
    res <- continue res
    equal (e_events res) [(0, 1, "4c"), (1, 1, "4d")]
    equal (has_integrated res) (Just True)

    -- Further integrates only cause one, thanks to the track cache.
    res <- next res $ UiTest.insert_event 2 (0, 0, "3c")
    equal (e_events res) [(0, 1, "4c"), (1, 1, "4d")]
    equal (has_integrated res) (Just True)
    res <- continue res
    equal (e_events res) [(0, 1, "3c"), (1, 1, "4d")]
    equal (has_integrated res) (Just False)

test_track_integrate_subblock :: Test
test_track_integrate_subblock = do
    let states = ResponderTest.mkstates_blocks
            [ ("top", [(">", [(0, 2, "sub")])])
            , ("sub", [(">i1", [(0, 1, ""), (1, 1, "")])])
            ]
    res <- start states $ Ui.set_track_title
        (UiTest.mk_tid_block (UiTest.bid "sub") 1) ">i1 | <"
    equal (e_events res) []
    pprint (e_integrated res)
    let ui = ResponderTest.result_ui_state res
    pprint (map (second (Block.block_integrated_tracks)) $
        Map.toList $ Ui.state_blocks ui)
    -- No errors.
    equal (map Log.msg_text (ResponderTest.result_logs res)) []

test_track_modify :: Test
test_track_modify = do
    let states = ResponderTest.mkstates $ UiTest.note_spec
            ("i1 | < | reverse", [(0, 1, "4c"), (1, 1, "4d")], [])

    res <- start states $ return ()
    equal (e_tracks res)
        [(UiTest.default_block_name,
            [ (">i1 | < | reverse", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            , (">i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4d"), (1, 0, "4c")])
            ])]
    -- Not derived yet.
    equal (e_events res) []

    res <- continue res
    equal (e_events res) [(0, 1, "4d"), (1, 1, "4c")]

    res <- next res $ UiTest.insert_event 2 (0, 0, "3c")
    equal (e_tracks res)
        [(UiTest.default_block_name,
            [ (">i1 | < | reverse", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "3c"), (1, 0, "4d")])
            , (">i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4d"), (1, 0, "3c")])
            ])]
    equal (e_events res) [(0, 1, "4d"), (1, 1, "4c")]
    res <- continue res
    equal (e_events res) [(0, 1, "4d"), (1, 1, "3c")]

    -- TODO make a call that creates more or fewer tracks

-- multiple integrations of different tracks
-- multiple integrations of the same track


-- * score integration

test_block_score_integrate :: Test
test_block_score_integrate = do
    let states = mkstates "" ("i1", [(0, 1, "4c"), (1, 1, "4d")], [])
    res <- start states $ do
        bid <- Create.block Ui.no_ruler
        Ui.set_integrated_block bid $
            Just (UiTest.default_block_id, Block.ScoreDestinations [])
    equal (e_tracks res)
        [ ("b1",
            [ (">i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ])
        , ("b2",
            [ (">i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ])
        ]
    res <- next res $ UiTest.insert_event 1 (4, 1, "")
    equal (e_tracks res)
        [ ("b1",
            [ (">i1", [(0, 1, ""), (1, 1, ""), (4, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ])
        , ("b2",
            [ (">i1", [(0, 1, ""), (1, 1, ""), (4, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ])
        ]

test_track_score_integrate :: Test
test_track_score_integrate = do
    let states = mkstates "" ("i1", [(0, 1, "4c")], [])
    res <- start states $ add_integrated_track 1
    equal (e_tracks res)
        [ (UiTest.default_block_name,
            [ (">i1", [(0, 1, "")]), ("*", [(0, 0, "4c")])
            , (">i1", [(0, 1, "")]), ("*", [(0, 0, "4c")])
            ])
        ]
    res <- next res $ UiTest.insert_event 1 (1, 1, "")
    equal (e_tracks res)
        [ (UiTest.default_block_name,
            [ (">i1", [(0, 1, ""), (1, 1, "")]), ("*", [(0, 0, "4c")])
            , (">i1", [(0, 1, ""), (1, 1, "")]), ("*", [(0, 0, "4c")])
            ])
        ]

test_score_integrate_two_tracks :: Test
test_score_integrate_two_tracks = do
    let states = mkstates_tracks ""
            [(">i1", [(0, 1, "a")]), (">i2", [(0, 1, "b")])]
    res <- start states $ add_integrated_track 1 >> add_integrated_track 2
    equal (e_tracks res)
        [ (UiTest.default_block_name,
            [ (">i1", [(0, 1, "a")]), (">i2", [(0, 1, "b")])
            , (">i1", [(0, 1, "a")]), (">i2", [(0, 1, "b")])
            ])
        ]
    equal (map List.sort (e_integrate_skeleton res))
        [[ (Config.score_integrate_skeleton, [(1, 3)])
         , (Config.score_integrate_skeleton, [(2, 4)])
        ]]

test_derive_integrate_twice :: Test
test_derive_integrate_twice = do
    let states = mkstates_tracks "" [(">i1", [(0, 1, "")])]
    res <- start states $ do
        Ui.set_track_title (UiTest.mk_tid 1) ">i1 | <"
    equal (e_tracks res)
        [ (UiTest.default_block_name,
            [ (">i1 | <", [(0, 1, "")])
            , (">i1", [(0, 1, "")])
            ])
        ]
    equal (map List.sort (e_integrate_skeleton res))
        [[ (Config.integrate_skeleton, [(1, 2)])
        ]]

    res <- next res $ do
        Ui.modify_integrated_tracks UiTest.default_block_id $
            ((UiTest.mk_tid 1, Block.DeriveDestinations []) :)
    equal (e_tracks res)
        [ (UiTest.default_block_name,
            [ (">i1 | <", [(0, 1, "")])
            , (">i1", [(0, 1, "")])
            , (">i1", [(0, 1, "")])
            ])
        ]
    equal (map List.sort (e_integrate_skeleton res))
        [[ (Config.integrate_skeleton, [(1, 2)])
         , (Config.integrate_skeleton, [(1, 3)])
        ]]

test_score_and_derive_integrate :: Test
test_score_and_derive_integrate = do
    let states = mkstates_tracks ""
            [(">i1", [(0, 1, "")]), (">i2", [(0, 1, "")])]
    res <- start states $ do
        add_integrated_track 1
        Ui.set_track_title (UiTest.mk_tid 2) ">i2 | <"
    equal (e_tracks res)
        [ (UiTest.default_block_name,
            [ (">i1", [(0, 1, "")])
            , (">i2 | <", [(0, 1, "")])
            , (">i1", [(0, 1, "")])
            , (">i2", [(0, 1, "")])
            ])
        ]
    equal (map List.sort (e_integrate_skeleton res))
        [[ (Config.score_integrate_skeleton, [(1, 3)])
         , (Config.integrate_skeleton, [(2, 4)])
        ]]

test_double_integrate :: Test
test_double_integrate = do
    let states = mkstates_tracks "" [(">i1", [(0, 1, "")])]
    res <- start states $
        Ui.set_track_title (UiTest.mk_tid 1) ">i1 | < | <"
    -- It threw but that winds up being logged.
    equal (e_tracks res)
        [(UiTest.default_block_name, [(">i1 | < | <", [(0, 1, "")])])]

add_integrated_track :: Ui.M m => TrackNum -> m ()
add_integrated_track tracknum =
    Ui.modify_integrated_tracks UiTest.default_block_id $
        ((UiTest.mk_tid tracknum, Block.ScoreDestinations []) :)

e_integrate_skeleton :: ResponderTest.Result
    -> [[(Color.Color, [Skeleton.Edge])]]
e_integrate_skeleton = map Block.integrate_skeleton . Map.elems
    . Ui.state_blocks . ResponderTest.result_ui_state

-- TODO these don't work, but this would test them if they did.
_test_cascading_track_score_integrate :: Test
_test_cascading_track_score_integrate = do
    let states = mkstates_tracks "" [(">i1", [(0, 1, "")])]
    res <- start states $ Ui.set_track_title (UiTest.mk_tid 1) ">i1 | <!"
    equal (e_tracks res)
        [ ("b1",
            [ (">i1 | <!", [(0, 1, "")])
            , (">i1", [(0, 1, "")])
            ])
        ]
    res <- next res $ Ui.set_track_title (UiTest.mk_tid 2) ">i1 | <!"
    equal (e_tracks res)
        [ ("b1",
            [ (">i1 | <!", [(0, 1, "")])
            , (">i1 | <!", [(0, 1, "")])
            , (">i1", [(0, 1, "")])
            ])
        ]

    -- Change should propagate through both.
    res <- next res $ UiTest.insert_event 1 (1, 1, "")
    equal (e_tracks res)
        [ ("b1",
            [ (">i1 | <!", [(0, 1, ""), (1, 1, "")])
            , (">i1 | <!", [(0, 1, ""), (1, 1, "")])
            , (">i1", [(0, 1, ""), (1, 1, "")])
            ])
        ]

-- * manual integration

test_manual_integrate :: Test
test_manual_integrate = do
    let key = "key"
    let f state note controls = UiTest.exec state $
            Integrate.manual_integrate key (mktrack note) (map mktrack controls)
        extract = first snd . UiTest.dump_block UiTest.default_block_id
    let initial = UiTest.exec Ui.empty $ do
            [tid1, tid2] <- UiTest.mkblock [(">", []), (">", [])]
            Ui.set_integrated_manual UiTest.default_block_id key $ Just
                [ Block.empty_destination "k1" tid1 []
                , Block.empty_destination "k2" tid2 []
                ]
    equal (extract initial) ([(">", []), (">", [])], [])
    let state = f initial (">i", [(0, 1, "hi")]) []
    equal (extract state) ([(">", [(0, 1, "hi")]), (">", [(0, 1, "hi")])], [])
    state <- return $ f state (">i", [(0, 1, "hi"), (1, 1, "there")])
        [("c", [(0, 1, "1")])]
    equal (extract state)
        ( [ (">", [(0, 1, "hi"), (1, 1, "there")]), ("c", [(0, 1, "1")])
          , (">", [(0, 1, "hi"), (1, 1, "there")]), ("c", [(0, 1, "1")])
          ]
        , [(1, 2), (3, 4)]
        )


type Event = (TrackTime, TrackTime, Text)

mktrack :: (Text, [Event]) -> Convert.Track
mktrack (title, events) = Convert.Track title (map mkevent events)

mkevent :: Event -> Event.Event
mkevent (start, dur, text) = Event.stack_ #= Just (mkstack start text) $
    Event.event start dur text

mkstack :: TrackTime -> Text -> Event.Stack
mkstack start text = Event.Stack (Stack.add (Stack.Call text) Stack.empty) start

-- * implementation

-- | Run a cmd and return the next DeriveComplete.
start :: ResponderTest.States -> Cmd.CmdT IO a -> IO ResponderTest.Result
start states action = last <$> until_complete states action

-- | Keep on getting results until all derives complete.
continue :: ResponderTest.Result -> IO ResponderTest.Result
continue = fmap last . ResponderTest.continue_all [] 1

-- | Run another cmd on the state returned from a previous one.
next :: ResponderTest.Result -> Cmd.CmdT IO a -> IO ResponderTest.Result
next = start . ResponderTest.result_states

next_without :: [BlockId] -> ResponderTest.Result -> Cmd.CmdT IO a
    -> IO ResponderTest.Result
next_without blocks_gone res = fmap last
    . ResponderTest.respond_all blocks_gone 1 (ResponderTest.result_states res)

nexts :: ResponderTest.Result -> Cmd.CmdT IO a -> IO [ResponderTest.Result]
nexts = until_complete . ResponderTest.result_states

until_complete :: ResponderTest.States -> Cmd.CmdT IO a
    -> IO [ResponderTest.Result]
until_complete = ResponderTest.respond_all [] 1

e_tracks :: ResponderTest.Result -> [UiTest.BlockSpec]
e_tracks = UiTest.extract_blocks . ResponderTest.result_ui_state

e_track_ids :: ResponderTest.Result -> [(BlockId, [TrackId])]
e_track_ids = UiTest.extract_track_ids . ResponderTest.result_ui_state

e_events :: ResponderTest.Result -> [(RealTime, RealTime, Text)]
e_events = map DeriveTest.e_note
    . CmdTest.e_events UiTest.default_block_id . ResponderTest.result_cmd

e_integrated :: ResponderTest.Result -> Maybe [Derive.Integrated]
e_integrated = fmap Cmd.perf_integrated . e_perf

e_perf :: ResponderTest.Result -> Maybe Cmd.Performance
e_perf = Map.lookup UiTest.default_block_id
    . Cmd.state_performance . Cmd.state_play . ResponderTest.result_cmd_state

mkstates :: Text -> UiTest.NoteSpec -> ResponderTest.States
mkstates title = mkstates_tracks title . UiTest.note_spec

mkstates_tracks :: Text -> [UiTest.TrackSpec] -> ResponderTest.States
mkstates_tracks title tracks = (UiTest.exec ui_state set_title, cmd_state)
    where
    (ui_state, cmd_state) = ResponderTest.mkstates tracks
    set_title = Ui.set_block_title UiTest.default_block_id title

run :: Text -> [UiTest.TrackSpec] -> Cmd.CmdId a -> CmdTest.Result a
run title tracks = CmdTest.run ustate CmdTest.default_cmd_state
    where
    ustate = UiTest.exec Ui.empty $ do
        UiTest.mkblock_view (UiTest.default_block_name, tracks)
        Ui.set_block_title UiTest.default_block_id title
