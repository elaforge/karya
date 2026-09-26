-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Integrate_test where
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Log as Log
import qualified Util.Thread as Thread
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
    res <- start states $ UiTest.insertEvent 1 (1, 1, "")
    -- create a new block, integrate b1 to b2
    equal (eTracks res)
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
        UiTest.insertEvent 1 (2, 1, "")
        UiTest.insertEvent 2 (2, 0, "4e")
    equal (last (eTracks res))
        ("b2",
            [ (">i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4e"), (1, 0, "4d"), (2, 0, "4c")])
            ])
    -- Delete an event in b2 and change b1.
    -- b1: 'cde' -> reverse to 'edc', remove 'd' so 'e c'
    -- b2: replace 'c' with 'f', so 'e f'
    reses <- nexts res $ do
        UiTest.removeEventIn "b2" 1 1
        UiTest.removeEventIn "b2" 2 1
        UiTest.insertEvent 2 (2, 0, "4f")
    equal (last (eTracks (last reses)))
        ("b2",
            [ (">i1", [(0, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4f"), (2, 0, "4c")])
            ])

test_block_integrate2 :: Test
test_block_integrate2 = do
    let states = mkstates "<<" ("i1", [(0, 1, "4c"), (1, 1, "4g")], [])
        source = "b1"
        dest = "b2"
        pitchTrack = fmap (snd . last) . lookup dest . eTracks
    res <- start states (pure ())
    res <- next res $ do
        UiTest.insertEventIn dest 2 (0.5, 0, "4a")
        UiTest.insertEventIn source 2 (0.25, 0, "4d")
    res <- next res $ UiTest.insertEventIn source 2 (0.5, 0, "4e")
    -- 4e in src is replaced by overidden by 4a in dest
    equal (pitchTrack res) $ Just
        [(0, 0, "4c"), (0.25, 0, "4d"), (0.5, 0, "4a"), (1, 0, "4g")]
    res <- next res $ UiTest.insertEventIn source 2 (0.75, 0, "4f")
    equal (pitchTrack res) $ Just
        [ (0, 0, "4c"), (0.25, 0, "4d"), (0.5, 0, "4a"), (0.75, 0, "4f")
        , (1, 0, "4g")
        ]

test_block_integrate_call_map :: Test
test_block_integrate_call_map = do
    let cmdState = CmdTest.mkCmdState $ UiTest.makeDb1 $
            MidiInst.attributeMap #= Patch.singleKeyswitches
                [(Attrs.attr "attr2", 42)] $
            MidiInst.common#Common.callMap #= Map.fromList
                [ (Attrs.attr "attr1", "one")
                ] $
            MidiInst.makePatch $ Patch.patch (-2, 2) "1"
    let uiState = UiTest.setInstruments ">i1 s/1 [ms] dev 1" $
            fst $ mkstates "<<"
                ( "i1"
                , [ (0, 1, "+attr1 -- 4c"), (1, 1, "+attr2 -- 4d")
                  , (2, 1, "+attr3 -- 4e")
                  ]
                , []
                )
    res <- start (uiState, cmdState) $ pure ()
    -- prettyp (e_integrated res)
    equal (eTracks res)
        [ ( "b1 -- <<"
          , UiTest.instNoteTrack1 "i1"
            ["+attr1 -- 4c", "+attr2 -- 4d", "+attr3 -- 4e"]
          )
        , ( "b2"
          , UiTest.instNoteTrack1 "i1"
            -- If I put the call map stuff back in Cmd.resolve_instrument,
            -- these +attr2 etc. will go back.
            ["one -- 4c", "+attr2 -- 4d", "+attr3 -- 4e"]
          )
        ]

test_track_integrate :: Test
test_track_integrate = do
    let states = ResponderTest.mkstates $ UiTest.noteSpec
            ("i1", [(0, 1, "4c"), (1, 1, "4d")], [])
        hasIntegrated = fmap (not . null) . eIntegrated
    -- The first track integrate causes two derives.
    res <- start states $ Ui.setTrackTitle (UiTest.mkTid 1) ">i1 | <"
    equal (eEvents res) []
    equal (hasIntegrated res) (Just True)
    res <- continue res
    equal (eEvents res) [(0, 1, "4c"), (1, 1, "4d")]
    equal (hasIntegrated res) (Just True)

    -- Further integrates only cause one, thanks to the track cache.
    res <- next res $ UiTest.insertEvent 2 (0, 0, "3c")
    equal (eEvents res) [(0, 1, "4c"), (1, 1, "4d")]
    equal (hasIntegrated res) (Just True)
    res <- continue res
    equal (eEvents res) [(0, 1, "3c"), (1, 1, "4d")]
    equal (hasIntegrated res) (Just False)

test_track_integrate_subblock :: Test
test_track_integrate_subblock = do
    let states = ResponderTest.mkstatesBlocks
            [ ("top", [(">", [(0, 2, "sub")])])
            , ("sub", [(">i1 | <", [(0, 1, ""), (1, 1, "")])])
            ]
    res <- start states (pure ())
    equal (eEvents res) []
    pprint (eIntegrated res)
    let ui = ResponderTest.resultUiState res
    pprint (map (second (Block.blockIntegratedTracks)) $
        Map.toList $ Ui.stateBlocks ui)
    -- No errors.
    equal (map Log.msgText (ResponderTest.resultLogs res)) []

test_track_integrate_unwarp :: Test
test_track_integrate_unwarp = do
    let run tracks = start (ResponderTest.mkstates tracks) (pure ())
    let track = (">i1 | < c", [(0, 0.5, "c=(next-event) |"), (0.5, 1, "")])
    res <- run [("tempo", [(0, 0, "1")]), track]
    -- Avoid stdout mixing with any debug trace from the last performance
    -- force.
    Thread.delay 0.1
    equal (eBlockTracks res) $ Just
        [ ("tempo", [(0, 0, "1")])
        , track
        , (">i1", [(0, 0.5, ""), (0.5, 1, "")])
        , ("c:s", [(0, 0, "`0x`80")])
        ]
    res <- run [("tempo", [(0, 0, ".5")]), track]
    Thread.delay 0.1
    equal (eBlockTracks res) $ Just
        [ ("tempo", [(0, 0, ".5")])
        , track
        , (">i1", [(0, 0.5, ""), (0.5, 1, "")])
        , ("c:s", [(0, 0, "`0x`ff")])
        ]

test_track_modify :: Test
test_track_modify = do
    let states = ResponderTest.mkstates $ UiTest.noteSpec
            ("i1 | < | reverse", [(0, 1, "4c"), (1, 1, "4d")], [])

    res <- start states $ pure ()
    equal (eTracks res)
        [(UiTest.defaultBlockName,
            [ (">i1 | < | reverse", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            , (">i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4d"), (1, 0, "4c")])
            ])]
    -- Not derived yet.
    equal (eEvents res) []

    res <- continue res
    equal (eEvents res) [(0, 1, "4d"), (1, 1, "4c")]

    res <- next res $ UiTest.insertEvent 2 (0, 0, "3c")
    equal (eTracks res)
        [(UiTest.defaultBlockName,
            [ (">i1 | < | reverse", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "3c"), (1, 0, "4d")])
            , (">i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4d"), (1, 0, "3c")])
            ])]
    equal (eEvents res) [(0, 1, "4d"), (1, 1, "4c")]
    res <- continue res
    equal (eEvents res) [(0, 1, "4d"), (1, 1, "3c")]

    -- TODO make a call that creates more or fewer tracks

-- multiple integrations of different tracks
-- multiple integrations of the same track


-- * score integration

test_block_score_integrate :: Test
test_block_score_integrate = do
    let states = mkstates "" ("i1", [(0, 1, "4c"), (1, 1, "4d")], [])
    res <- start states $ do
        bid <- Create.block Ui.noRuler
        Ui.setIntegratedBlock bid $
            Just (UiTest.defaultBlockId, Block.ScoreDestinations [])
    equal (eTracks res)
        [ ("b1",
            [ (">i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ])
        , ("b2",
            [ (">i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ])
        ]
    res <- next res $ UiTest.insertEvent 1 (4, 1, "")
    equal (eTracks res)
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
    res <- start states $ addIntegratedTrack 1
    equal (eTracks res)
        [ (UiTest.defaultBlockName,
            [ (">i1", [(0, 1, "")]), ("*", [(0, 0, "4c")])
            , (">i1", [(0, 1, "")]), ("*", [(0, 0, "4c")])
            ])
        ]
    res <- next res $ UiTest.insertEvent 1 (1, 1, "")
    equal (eTracks res)
        [ (UiTest.defaultBlockName,
            [ (">i1", [(0, 1, ""), (1, 1, "")]), ("*", [(0, 0, "4c")])
            , (">i1", [(0, 1, ""), (1, 1, "")]), ("*", [(0, 0, "4c")])
            ])
        ]

test_score_integrate_two_tracks :: Test
test_score_integrate_two_tracks = do
    let states = mkstatesTracks ""
            [(">i1", [(0, 1, "a")]), (">i2", [(0, 1, "b")])]
    res <- start states $ addIntegratedTrack 1 >> addIntegratedTrack 2
    equal (eTracks res)
        [ (UiTest.defaultBlockName,
            [ (">i1", [(0, 1, "a")]), (">i2", [(0, 1, "b")])
            , (">i1", [(0, 1, "a")]), (">i2", [(0, 1, "b")])
            ])
        ]
    equal (map List.sort (eIntegrateSkeleton res))
        [[ (Config.scoreIntegrateSkeleton, [(1, 3)])
         , (Config.scoreIntegrateSkeleton, [(2, 4)])
        ]]

test_derive_integrate_twice :: Test
test_derive_integrate_twice = do
    let states = mkstatesTracks "" [(">i1", [(0, 1, "")])]
    res <- start states $ do
        Ui.setTrackTitle (UiTest.mkTid 1) ">i1 | <"
    equal (eTracks res)
        [ (UiTest.defaultBlockName,
            [ (">i1 | <", [(0, 1, "")])
            , (">i1", [(0, 1, "")])
            ])
        ]
    equal (map List.sort (eIntegrateSkeleton res))
        [[ (Config.integrateSkeleton, [(1, 2)])
        ]]

    res <- next res $ do
        Ui.modifyIntegratedTracks UiTest.defaultBlockId $
            ((UiTest.mkTid 1, Block.DeriveDestinations []) :)
    equal (eTracks res)
        [ (UiTest.defaultBlockName,
            [ (">i1 | <", [(0, 1, "")])
            , (">i1", [(0, 1, "")])
            , (">i1", [(0, 1, "")])
            ])
        ]
    equal (map List.sort (eIntegrateSkeleton res))
        [[ (Config.integrateSkeleton, [(1, 2)])
         , (Config.integrateSkeleton, [(1, 3)])
        ]]

test_score_and_derive_integrate :: Test
test_score_and_derive_integrate = do
    let states = mkstatesTracks ""
            [(">i1", [(0, 1, "")]), (">i2", [(0, 1, "")])]
    res <- start states $ do
        addIntegratedTrack 1
        Ui.setTrackTitle (UiTest.mkTid 2) ">i2 | <"
    equal (eTracks res)
        [ (UiTest.defaultBlockName,
            [ (">i1", [(0, 1, "")])
            , (">i2 | <", [(0, 1, "")])
            , (">i1", [(0, 1, "")])
            , (">i2", [(0, 1, "")])
            ])
        ]
    equal (map List.sort (eIntegrateSkeleton res))
        [[ (Config.scoreIntegrateSkeleton, [(1, 3)])
         , (Config.integrateSkeleton, [(2, 4)])
        ]]

test_double_integrate :: Test
test_double_integrate = do
    let states = mkstatesTracks "" [(">i1", [(0, 1, "")])]
    res <- start states $
        Ui.setTrackTitle (UiTest.mkTid 1) ">i1 | < | <"
    -- It threw but that winds up being logged.
    equal (eTracks res)
        [(UiTest.defaultBlockName, [(">i1 | < | <", [(0, 1, "")])])]

addIntegratedTrack :: Ui.M m => TrackNum -> m ()
addIntegratedTrack tracknum =
    Ui.modifyIntegratedTracks UiTest.defaultBlockId $
        ((UiTest.mkTid tracknum, Block.ScoreDestinations []) :)

eIntegrateSkeleton :: ResponderTest.Result
    -> [[(Color.Color, [Skeleton.Edge])]]
eIntegrateSkeleton = map Block.integrateSkeleton . Map.elems
    . Ui.stateBlocks . ResponderTest.resultUiState

-- TODO these don't work, but this would test them if they did.
_testCascadingTrackScoreIntegrate :: Test
_testCascadingTrackScoreIntegrate = do
    let states = mkstatesTracks "" [(">i1", [(0, 1, "")])]
    res <- start states $ Ui.setTrackTitle (UiTest.mkTid 1) ">i1 | <!"
    equal (eTracks res)
        [ ("b1",
            [ (">i1 | <!", [(0, 1, "")])
            , (">i1", [(0, 1, "")])
            ])
        ]
    res <- next res $ Ui.setTrackTitle (UiTest.mkTid 2) ">i1 | <!"
    equal (eTracks res)
        [ ("b1",
            [ (">i1 | <!", [(0, 1, "")])
            , (">i1 | <!", [(0, 1, "")])
            , (">i1", [(0, 1, "")])
            ])
        ]

    -- Change should propagate through both.
    res <- next res $ UiTest.insertEvent 1 (1, 1, "")
    equal (eTracks res)
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
            Integrate.manualIntegrate key (mktrack note) (map mktrack controls)
        extract = first snd . UiTest.dumpBlock UiTest.defaultBlockId
    let initial = UiTest.exec Ui.empty $ do
            [tid1, tid2] <- UiTest.mkblock [(">", []), (">", [])]
            Ui.setIntegratedManual UiTest.defaultBlockId key $ Just
                [ Block.emptyDestination "k1" tid1 []
                , Block.emptyDestination "k2" tid2 []
                ]
    equal (extract initial) ([(">", []), (">", [])], [])
    let state = f initial (">i", [(0, 1, "hi")]) []
    equal (extract state) ([(">", [(0, 1, "hi")]), (">", [(0, 1, "hi")])], [])
    state <- pure $ f state (">i", [(0, 1, "hi"), (1, 1, "there")])
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
start states action = last <$> untilComplete states action

-- | Keep on getting results until all derives complete.
continue :: ResponderTest.Result -> IO ResponderTest.Result
continue = fmap last . ResponderTest.continueAll [] 1

-- | Run another cmd on the state returned from a previous one.
next :: ResponderTest.Result -> Cmd.CmdT IO a -> IO ResponderTest.Result
next = start . ResponderTest.resultStates

nextWithout :: [BlockId] -> ResponderTest.Result -> Cmd.CmdT IO a
    -> IO ResponderTest.Result
nextWithout blocksGone res = fmap last
    . ResponderTest.respondAll blocksGone 1 (ResponderTest.resultStates res)

nexts :: ResponderTest.Result -> Cmd.CmdT IO a -> IO [ResponderTest.Result]
nexts = untilComplete . ResponderTest.resultStates

untilComplete :: ResponderTest.States -> Cmd.CmdT IO a
    -> IO [ResponderTest.Result]
untilComplete = ResponderTest.respondAll [] 1

eTracks :: ResponderTest.Result -> [UiTest.BlockSpec]
eTracks = UiTest.extractBlocks . ResponderTest.resultUiState

eBlockTracks :: ResponderTest.Result -> Maybe [UiTest.TrackSpec]
eBlockTracks = lookup UiTest.defaultBlockName . eTracks

eTrackIds :: ResponderTest.Result -> [(BlockId, [TrackId])]
eTrackIds = UiTest.extractTrackIds . ResponderTest.resultUiState

eEvents :: ResponderTest.Result -> [(RealTime, RealTime, Text)]
eEvents = map DeriveTest.eNote
    . CmdTest.eEvents UiTest.defaultBlockId . ResponderTest.resultCmd

eIntegrated :: ResponderTest.Result -> Maybe [Derive.Integrated]
eIntegrated = fmap Cmd.perfIntegrated . ePerf

ePerf :: ResponderTest.Result -> Maybe Cmd.Performance
ePerf = Map.lookup UiTest.defaultBlockId
    . Cmd.statePerformance . Cmd.statePlay . ResponderTest.resultCmdState

mkstates :: Text -> UiTest.NoteSpec -> ResponderTest.States
mkstates title = mkstatesTracks title . UiTest.noteSpec

mkstatesTracks :: Text -> [UiTest.TrackSpec] -> ResponderTest.States
mkstatesTracks title tracks = (UiTest.exec uiState setTitle, cmdState)
    where
    (uiState, cmdState) = ResponderTest.mkstates tracks
    setTitle = Ui.setBlockTitle UiTest.defaultBlockId title

run :: Text -> [UiTest.TrackSpec] -> Cmd.CmdId a -> CmdTest.Result a
run title tracks = CmdTest.run ustate CmdTest.defaultCmdState
    where
    ustate = UiTest.exec Ui.empty $ do
        UiTest.mkblockView (UiTest.defaultBlockName, tracks)
        Ui.setBlockTitle UiTest.defaultBlockId title
