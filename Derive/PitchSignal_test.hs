-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.PitchSignal_test where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Test
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


test_apply_controls = do
    let f controls psig = unsignal $ PitchSignal.apply_controls
            (mkcontrols controls) (mksignal psig)
        mkcontrols = Map.fromList . map (second (Score.untyped . Signal.signal))
        err = Left "bad transpose"
    -- No effect.
    equal (f [("normal", [(1, 1), (2, 2), (3, 3)])] [(0, 1)]) [(0, Right 1)]
    equal (f [(trans1, [(1, 1), (2, 2), (3, 3)])] [(0, 2)])
        [(0, Right 2), (1, Right 3), (2, err), (3, err)]
    equal (f [(trans1, [(0, 0), (1, -1), (2, 0)])] [(0, 2)])
        [(0, Right 2), (1, Right 1), (2, Right 2)]

    equal (f [(trans1, [(1, 1), (3, 3)]), (trans2, [(0, 0)])] [(1, 0)])
        [(0, Right 0), (1, Right 1), (3, Right 3)]
    equal (f [(trans1, [(1, 1), (3, 3)]), (trans2, [(0, 0), (2, 2)])] [(1, 0)])
        [(0, Right 0), (1, Right 1), (2, Right 1), (3, Right 3)]

mksignal :: [(RealTime, Pitch.NoteNumber)] -> PitchSignal.Signal
mksignal = PitchSignal.signal . map (second mkpitch)

default_scale :: PitchSignal.Scale
default_scale = PitchSignal.Scale "test" (Set.fromList [trans1, trans2])

mkpitch :: Pitch.NoteNumber -> PitchSignal.Pitch
mkpitch nn =
    PitchSignal.pitch default_scale note (const $ Right $ Pitch.Note $ showt nn)
        mempty
    where
    note config
        | nn + t >= 4 = Left (PitchSignal.PitchError "bad transpose")
        | otherwise = Right (nn + t)
        where
        t = Pitch.NoteNumber $ Map.findWithDefault 0 trans1
            (PitchSignal.pitch_controls config)

trans1, trans2 :: Score.Control
trans1 = "trans1"
trans2 = "trans2"

unsignal :: PitchSignal.Signal -> [(RealTime, Either String Pitch.NoteNumber)]
unsignal = map (second (unerror . PitchSignal.pitch_nn . PitchSignal.coerce))
        . PitchSignal.unsignal
    where unerror = either (\(PitchSignal.PitchError s) -> Left (untxt s)) Right
