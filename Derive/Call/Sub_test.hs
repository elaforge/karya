-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Sub_test where
import qualified Derive.Args as Args
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.NoteUtil as NoteUtil
import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import qualified Derive.Slice_test as Slice_test

import qualified Perform.NN as NN
import qualified Perform.Signal as Signal
import qualified Ui.Event as Event
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_inverting :: Test
test_inverting = do
    let run = DeriveTest.extractEvents extract
            . DeriveTest.deriveTracksLinear ""
        extract e = (DeriveTest.eNote e, DeriveTest.eAttributes e)
    -- Simple inversion.
    equal (run [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")])])
        [((0, 1, "4c"), "+")]
    -- Make sure empty tracks are stripped.  But the title of the empty track
    -- is still applied.
    equal (run [(">", [(0, 1, "")]), ("> | +a", []), ("*", [(0, 0, "4c")])])
        [((0, 1, "4c"), "+a")]
    -- Initial empty track is also stripped.
    equal (run [(">", []), (">", [(0, 1, "")]), ("*", [(0, 0, "4c")])])
        [((0, 1, "4c"), "+")]

test_inverting_block :: Test
test_inverting_block = do
    let run = DeriveTest.extract DeriveTest.eNote
            . DeriveTest.deriveBlocks
    -- This used to fail when I used an Inverted state instead of stripping
    -- the subtracks in 'Sub.inverting'.
    equal (run
            [ ("top", [(">", [(0, 1, "sub")]), ("*", [])])
            , ("sub=ruler", UiTest.regularNotes 1)
            ])
        ([(0, 1, "3c")], [])

test_under_invert :: Test
test_under_invert = do
    let run underInvert = DeriveTest.extract (DeriveTest.eControlVals "out")
            . DeriveTest.deriveTracksSetup (call underInvert) ""
        call underInvert =
            CallTest.withNoteTransformer "t" (trans underInvert)
            <> CallTest.withNoteGenerator "g" gen

    -- A normal call sees the outer "c".
    equal (run False
            [ ("c", [(0, 0, "1")])
            , (">", [(0, 1, "t |")])
            , ("c", [(0, 0, ".5")])
            ])
        ([[1]], [])
    -- under_invert sees the inner "c".
    equal (run True
            [ ("c", [(0, 0, "1")])
            , (">", [(0, 1, "t |")])
            , ("c", [(0, 0, ".5")])
            ])
        ([[0.5]], [])

    -- under_invert works even with no inversion.
    equal (run True
            [ ("c", [(0, 0, "1")])
            , (">", [(0, 1, "t |")])
            ])
        ([[1]], [])

    -- And for a generator with subs that still doesn't want to invert.
    equal (run True
            [ ("c", [(0, 0, "1")])
            , (">", [(0, 1, "t | g")])
            , ("c", [(0, 0, ".5")])
            ])
        ([[]], [])
        -- Well, except the transformer gets lost, see 'Sub.under_invert'.
        -- ([[(0, 1)]], [])
    where
    trans underInvert = CallTest.transformer $
        (if underInvert then Sub.underInvert else ($)) $
        \args deriver -> do
            val <- Derive.untypedControlAt "c" =<< Args.realStart args
            let sig = ScoreT.untyped (Signal.constant (fromMaybe 0 val))
            Derive.withControl "out" sig deriver
    gen :: Derive.Generator Derive.Note
    gen = Derive.generator1 "mod" "gen" mempty "doc" $ Sig.call0 $ \args -> do
        dyn <- Internal.getDynamic id
        NoteUtil.makeEvent args dyn 0 1 "" mempty

test_inverted_control_scope :: Test
test_inverted_control_scope = do
    let run = DeriveTest.extract DeriveTest.eNns
            . DeriveTest.deriveTracks ""
    -- Inverted controls are clipped to their note's extent.
    equal (run $ UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])
        ([[(0, 60), (1, 60)], [(1, 62)]], [])
    equal (run $ UiTest.noteTrack [(0, 1, "+a -- 4c"), (1, 1, "4d")])
        ([[(0, 60), (1, 60)], [(1, 62)]], [])

test_overlapping_parent_control_scope :: Test
test_overlapping_parent_control_scope = do
    let run = DeriveTest.extract DeriveTest.eNns
            . DeriveTest.deriveTracksLinear "import ly"
    --          0 1 2 3 4
    -- >        v=1-----|
    -- > | ly-t v=2-|
    -- >        --|-|---|
    -- *        c d e

    -- ly-track evaluates sub-events, which means a slice from (0, 4).  The
    -- range is (0, 4), but since there is an orphan note starting at 2,
    -- I should actually trim the signal to (0, 2).  The key is that inverted
    -- controls only belong to one note, and the 4e is already beneath the
    -- orphan event.  Maybe I should make the slice contract so it doesn't
    -- cover orphans.
    equal (run
            [ (">", [(0, 4, "v=1")])
            , ("> | ly-track", [(0, 2, "v=2")])
            , (">", [(0, 1, ""), (1, 1, ""), (2, 2, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4e")])
            ])
        ( [ [(0, NN.c4), (1, NN.c4)], [(1, NN.d4), (2, NN.d4), (2, NN.e4)]
            , [(2, NN.e4)]
            ]
          , []
        )
        -- TODO should be
        -- ([[(0, NN.c4)], [(1, NN.d4)], [(2, NN.e4)]], [])

test_sub_notes :: Test
test_sub_notes = do
    let run tracks = DeriveTest.extract extract $
            DeriveTest.deriveTracksLinear "" tracks
        extract = DeriveTest.eAttributes
    -- Ensure zero duration subs show up.
    equal (run [(">", [(0, 2, "+a")]), (">", [(0, 0, ""), (1, 0, "")])])
        (["+a", "+a"], [])
    -- It should show up only once!
    equal (run
            [(">", [(0, 0, "+a")]), (">", [(0, 0, "+b")]), (">", [(0, 1, "")])])
        (["+a+b"], [])

makeControls :: String -> [Int] -> (String, [Slice_test.Event])
makeControls title ps = (title, [(toScore p, 0, showt p) | p <- ps])
    where toScore = ScoreTime.fromDouble . fromIntegral

mkargs :: Text -> [Slice_test.EventsTree] -> Derive.PassedArgs d
mkargs text subs = Derive.PassedArgs [] "call" info
    where
    event = Event.event 0 1 text
    info = Derive.Context
        { Derive.ctxPrevVal = Nothing
        , Derive.ctxEvent = event
        , Derive.ctxPrevEvents = prev
        , Derive.ctxNextEvents = next
        , Derive.ctxEventEnd = eventEnd
        , Derive.ctxTrackShifted = 0
        , Derive.ctxSubTracks = map Slice_test.makeTree subs
        , Derive.ctxSubEvents = Nothing
        , Derive.ctxTrackType = Nothing
        }
    prev = []
    next = [Event.event (Event.end event) 0 "next"]
    eventEnd = 100
