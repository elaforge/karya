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


print_note_numbers :: Pitch.ScaleId -> IO ()
print_note_numbers scale = do
    let run scale_id environ = DeriveTest.eval Ui.empty
            (scale_degrees scale_id environ)
    prettyp (run scale mempty)

scale_degrees :: Pitch.ScaleId -> Env.Environ
    -> Derive.Deriver [(Pitch.Pitch, Pitch.Note, Pitch.NoteNumber)]
scale_degrees scale_id environ = do
    scale <- Derive.get_scale scale_id
    nns <- Scale.note_numbers scale environ
    return $ zip3 (Scale.pitches scale environ) (Scale.notes scale environ) nns

test_assign_keys = do
    let f = Scale.assign_keys 4
    equal (f [1.5]) [(1, 1.5)]
    equal (f [1.5, 4]) [(1, 1.5), (3, 4)]
    equal (f [1, 2, 3, 4]) [(0, 1), (1, 2), (2, 3), (3, 4)]
    equal (f [1, 2, 3, 4, 5]) [(0, 1), (1, 2), (2, 3), (3, 4)]
    equal (f [2, 3, 4]) [(1, 2), (2, 3), (3, 4)]
