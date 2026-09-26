-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale_test where
import Util.Test
import qualified Ui.Ui as Ui
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.Scale as Scale

import qualified Perform.Pitch as Pitch


printNoteNumbers :: Pitch.ScaleId -> IO ()
printNoteNumbers scale = do
    let run scaleId environ = DeriveTest.eval Ui.empty
            (scaleDegrees scaleId environ)
    prettyp (run scale mempty)

scaleDegrees :: Pitch.ScaleId -> Env.Environ
    -> Derive.Deriver [(Pitch.Pitch, Pitch.Note, Pitch.NoteNumber)]
scaleDegrees scaleId environ = do
    scale <- Derive.getScale scaleId
    nns <- Scale.noteNumbers scale environ
    pure $ zip3 (Scale.pitches scale environ) (Scale.notes scale environ) nns

test_assign_keys :: Test
test_assign_keys = do
    let f = Scale.assignKeys 4
    equal (f [1.5]) [(1, 1.5)]
    equal (f [1.5, 4]) [(1, 1.5), (3, 4)]
    equal (f [1, 2, 3, 4]) [(0, 1), (1, 2), (2, 3), (3, 4)]
    equal (f [1, 2, 3, 4, 5]) [(0, 1), (1, 2), (2, 3), (3, 4)]
    equal (f [2, 3, 4]) [(1, 2), (2, 3), (3, 4)]
