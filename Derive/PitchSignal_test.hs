module Derive.PitchSignal_test where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import Util.Test
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


test_apply_controls = do
    let f cont sig psig = unsignal $ PitchSignal.apply_controls
            (Map.singleton cont (Score.untyped (Signal.signal sig)))
            (mksignal psig)
        err = Left "bad transpose"
    -- No effect.
    equal (f c_normal [(1, 1), (2, 2), (3, 3)] [(0, 1)]) [(0, Right 1)]
    equal (f c_trans [(1, 1), (2, 2), (3, 3)] [(0, 2)])
        [(0, Right 2), (1, Right 3), (2, err), (3, err)]
    equal (f c_trans [(0, 0), (1, -1), (2, 0)] [(0, 2)])
        [(0, Right 2), (1, Right 1), (2, Right 2)]

mksignal :: [(RealTime, Pitch.NoteNumber)] -> PitchSignal.Signal
mksignal = PitchSignal.signal (Pitch.ScaleId "test", Set.fromList [c_trans])
    . map (second mkpitch)

mkpitch :: Pitch.NoteNumber -> PitchSignal.Pitch
mkpitch nn = PitchSignal.pitch $ \controls -> do
    let t = Pitch.NoteNumber $ maybe 0 Score.typed_val $
            Map.lookup c_trans controls
    if nn + t >= 4 then Left (PitchSignal.PitchError "bad transpose")
        else Right (nn + t)

c_normal, c_trans :: Score.Control
c_normal = Score.Control "normal"
c_trans = Score.Control "trans"

unsignal :: PitchSignal.Signal -> [(RealTime, Either String Pitch.NoteNumber)]
unsignal = map (second (unerror . PitchSignal.pitch_nn)) . PitchSignal.unsignal
    where unerror = either (\(PitchSignal.PitchError s) -> Left s) Right
