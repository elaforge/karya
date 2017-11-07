-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Sub_test where
import Util.Test
import qualified Ui.Event as Event
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.UiTest as UiTest

import qualified Derive.Args as Args
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Slice_test as Slice_test

import qualified Perform.NN as NN
import qualified Perform.Signal as Signal
import Global


test_inverting = do
    let run = DeriveTest.extract_events extract
            . DeriveTest.derive_tracks_linear ""
        extract e = (DeriveTest.e_note e, DeriveTest.e_attributes e)
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

test_inverting_block = do
    let run = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_blocks
    -- This used to fail when I used an Inverted state instead of stripping
    -- the subtracks in 'Sub.inverting'.
    equal (run
            [ ("top", [(">", [(0, 1, "sub")]), ("*", [])])
            , ("sub=ruler", UiTest.regular_notes 1)
            ])
        ([(0, 1, "3c")], [])

test_under_invert = do
    let run under_invert = DeriveTest.extract (DeriveTest.e_control "out")
            . DeriveTest.derive_tracks_setup (call under_invert) ""
        call under_invert =
            CallTest.with_note_transformer "t" (trans under_invert)
            <> CallTest.with_note_generator "g" gen

    -- A normal call sees the outer "c".
    equal (run False
            [ ("c", [(0, 0, "1")])
            , (">", [(0, 1, "t |")])
            , ("c", [(0, 0, ".5")])
            ])
        ([[(0, 1)]], [])
    -- under_invert sees the inner "c".
    equal (run True
            [ ("c", [(0, 0, "1")])
            , (">", [(0, 1, "t |")])
            , ("c", [(0, 0, ".5")])
            ])
        ([[(0, 0.5)]], [])

    -- under_invert works even with no inversion.
    equal (run True
            [ ("c", [(0, 0, "1")])
            , (">", [(0, 1, "t |")])
            ])
        ([[(0, 1)]], [])

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
    trans under_invert =
        Derive.transformer "mod" "trans" mempty "doc" $ Sig.call0t $
        (if under_invert then Sub.under_invert else ($)) $
        \args deriver -> do
            val <- Derive.untyped_control_at "c" =<< Args.real_start args
            let sig = Score.untyped (Signal.constant (fromMaybe 0 val))
            Derive.with_control "out" sig deriver
    gen :: Derive.Generator Derive.Note
    gen = Derive.generator1 "mod" "gen" mempty "doc" $ Sig.call0 $ \args -> do
        dyn <- Internal.get_dynamic id
        Note.make_event args dyn 0 1 mempty

test_inverted_control_scope = do
    let run = DeriveTest.extract DeriveTest.e_nns . DeriveTest.derive_tracks ""
    -- Inverted controls are clipped to their note's extent.
    equal (run $ UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
        ([[(0, 60)], [(1, 62)]], [])
    equal (run $ UiTest.note_track [(0, 1, "+a -- 4c"), (1, 1, "4d")])
        ([[(0, 60)], [(1, 62)]], [])

test_overlapping_parent_control_scope = do
    let run = DeriveTest.extract DeriveTest.e_nns
            . DeriveTest.derive_tracks_linear "import ly"
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
        ([[(0, NN.c4)], [(1, NN.d4), (2, NN.e4)], [(2, NN.e4)]], [])
        -- TODO should be
        -- ([[(0, NN.c4)], [(1, NN.d4)], [(2, NN.e4)]], [])

test_sub_notes = do
    let run tracks = DeriveTest.extract extract $
            DeriveTest.derive_tracks_linear "" tracks
        extract = DeriveTest.e_attributes
    -- Ensure zero duration subs show up.
    equal (run [(">", [(0, 2, "+a")]), (">", [(0, 0, ""), (1, 0, "")])])
        (["+a", "+a"], [])
    -- It should show up only once!
    equal (run
            [(">", [(0, 0, "+a")]), (">", [(0, 0, "+b")]), (">", [(0, 1, "")])])
        (["+a+b"], [])

make_controls :: String -> [Int] -> (String, [Slice_test.Event])
make_controls title ps = (title, [(to_score p, 0, showt p) | p <- ps])
    where to_score = ScoreTime.double . fromIntegral

mkargs :: Text -> [Slice_test.EventsTree] -> Derive.PassedArgs d
mkargs text subs = Derive.PassedArgs [] "call" info
    where
    event = Event.event 0 1 text
    info = Derive.Context
        { Derive.ctx_prev_val = Nothing
        , Derive.ctx_event = event
        , Derive.ctx_prev_events = prev
        , Derive.ctx_next_events = next
        , Derive.ctx_event_end = event_end
        , Derive.ctx_track_shifted = 0
        , Derive.ctx_sub_tracks = map Slice_test.make_tree subs
        , Derive.ctx_sub_events = Nothing
        , Derive.ctx_track_type = Nothing
        }
    prev = []
    next = [Event.event (Event.end event) 0 "next"]
    event_end = 100
