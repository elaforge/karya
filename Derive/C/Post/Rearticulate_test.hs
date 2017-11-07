-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Post.Rearticulate_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import Types


test_slur_n = do
    let run title n = DeriveTest.extract e_note_nn $
            DeriveTest.derive_tracks title $ UiTest.regular_notes n
    equal (run "curve-time=0 | slur-n 2" 4)
        ([ (0, 2, [(0, NN.c3), (1, NN.d3), (2, NN.d3)])
         , (2, 2, [(2, NN.e3), (3, NN.f3), (4, NN.f3)])
         ], [])
    equal (run "srate=.5 | curve-time=1 | slur-n 2" 2)
        ([(0, 2, [(0, NN.c3), (1, NN.cs3), (1.5, NN.d3), (2, NN.d3)])], [])
    -- curve-time is limited by the duration of the notes.
    equal (run "srate=.5 | curve-time=2 | slur-n 2" 2)
        ([(0, 2, [(0, NN.c3), (1, NN.cs3), (1.5, NN.d3), (2, NN.d3)])], [])

test_slur_dur = do
    let run title n = DeriveTest.extract e_note_nn $
            DeriveTest.derive_tracks title $ UiTest.regular_notes n
    equal (run "curve-time=0 | slur-dur 2 1" 6)
        ([ (0, 1, [(0, NN.c3), (1, NN.c3)])
         , (1, 2, [(1, NN.d3), (2, NN.e3), (3, NN.e3)])
         , (3, 2, [(3, NN.f3), (4, NN.g3), (5, NN.g3)])
         , (5, 1, [(5, NN.a3), (6, NN.a3)])
         ], [])

e_note_nn :: Score.Event -> (RealTime, RealTime, [(RealTime, Pitch.NoteNumber)])
e_note_nn e = (Score.event_start e, Score.event_duration e, DeriveTest.e_nns e)
