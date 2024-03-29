-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Note_test where
import qualified Data.Map as Map

import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest

import qualified Instrument.Common as Common
import qualified Perform.Midi.Patch as Patch
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_note_track_call :: Test
test_note_track_call = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks_setup
                (CallTest.with_note_transformer ">i1" trans) ""
        extract e = DeriveTest.e_environ_val "x" e :: Maybe Int
        trans = CallTest.transformer $ \_ -> Derive.with_val "x" (42 :: Int)
    equal (run [(">i1", [(0, 1, "")])]) ([Just 42], [])
    equal (run [(">i2", [(0, 1, "")])]) ([Nothing], [])

test_sustain_controls :: Test
test_sustain_controls = do
    let run title = DeriveTest.extract DeriveTest.e_start_dur
            . DeriveTest.derive_tracks title . UiTest.note_track $ [(0, 1, "")]
    equal (run "") ([(0, 1)], [])
    equal (run "sus-set = .25") ([(0, 0.25)], [])
    equal (run "sus-abs = .25") ([(0, 1.25)], [])
    equal (run "sus = .5") ([(0, 0.5)], [])
    -- First sus, then sus-abs.
    equal (run "sus=.5 | sus-abs=-.25") ([(0, 0.25)], [])
    -- sus-set overrides them all.
    equal (run "sus=.5 | sus-abs=-.25 | sus-set=.75") ([(0, 0.75)], [])
    equal (run ".") ([(0, 0.5)], [])
    -- First staccato will override sus-abs.  Not sure why, but it was on
    -- purpose.
    equal (run ". | sus-abs=-.25") ([(0, 0.5)], [])
    -- sus and staccato multiply.
    equal (run ". | sus=.5") ([(0, 0.25)], [])

test_orphan_notes :: Test
test_orphan_notes = do
    -- Slice out orphans that aren't covered by a parent event.
    -- Also tested in 'Derive.Slice_test.test_slice_notes_orphans'.
    -- This is analogous to track level orphans, which are tested in
    -- "Ui.Call.BlockUtil_test".
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks_linear ""
        extract e = (DeriveTest.e_note e, DeriveTest.e_attributes e)
    equal (run
        [ (">i", [(0, 2, "a = b")])
        , (">", [])
        , (">", [(0, 1, ""), (1, 1, "")])
        , ("*", [(0, 0, "4c"), (1, 0, "4d")])
        ])
        ([((0, 1, "4c"), "+"), ((1, 1, "4d"), "+")], [])
    equal (run
        [ (">i", [(0, 2, "a = b")])
        , (">", [(0, 1, "+a")])
        , (">", [(0, 1, ""), (1, 1, "")])
        , ("*", [(0, 0, "4c"), (1, 0, "4d")])
        ])
        ([((0, 1, "4c"), "+a"), ((1, 1, "4d"), "+")], [])

test_transpose :: Test
test_transpose = do
    let run = DeriveTest.extract DeriveTest.e_pitch
            . DeriveTest.derive_tracks ""
    equal (run [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")]),
            ("t-chrom", [(0, 0, "1")])])
        (["4c#"], [])

test_apply_instrument_controls :: Test
test_apply_instrument_controls = do
    let run title controls = DeriveTest.extract DeriveTest.e_pitch $
            DeriveTest.derive_tracks_setup (with_config controls) title
                [(">i1", [(0, 1, "")]), ("*", [(0, 0, "4c")])]
        with_config controls = DeriveTest.with_midi_config "i1" "s/1"
            (Common.controls #= controls $ Common.empty_config)
            (Patch.config [])
    -- This doesn't test the controls directly, but rather that the
    -- transposition is applied as expected.
    equal (run "" mempty) (["4c"], [])
    let octave_up = Map.fromList [(Controls.octave, 1)]
    equal (run "" octave_up) (["5c"], [])
    -- Controls are merged with their default mergers.
    equal (run "t-oct=1" octave_up) (["6c"], [])
    -- Controls don't get applied multiple times.
    equal (run "inst = i1" octave_up) (["5c"], [])
