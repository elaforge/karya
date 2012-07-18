-- | Utilities for 'PitchSignal.Signal's and 'PitchSignal.Pitch's.
--
-- Functions here can't go into "Derive.PitchSignal" itself due to circular
-- imports---PitchSignal is a low level module imported by other low level
-- modules like "Derive.Score".
module Derive.Pitches where
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import Types


signal :: Scale.Scale -> [(RealTime, PitchSignal.Pitch)] -> PitchSignal.Signal
signal scale = PitchSignal.signal (Derive.pitch_signal_scale scale)

-- | A pitch interpolated a certain distance between two other pitches.
interpolated :: PitchSignal.Pitch -> PitchSignal.Pitch -> Double
    -> PitchSignal.Pitch
interpolated low high dist = PitchSignal.pitch $ \controls -> do
    low_nn <- PitchSignal.eval_pitch low controls
    high_nn <- PitchSignal.eval_pitch high controls
    return $ Num.scale low_nn high_nn (Pitch.NoteNumber dist)

-- | Transpose a pitch.
transpose :: Pitch.Transpose -> PitchSignal.Pitch -> PitchSignal.Pitch
transpose (Pitch.Chromatic v) =
    PitchSignal.add_control Score.c_chromatic v
transpose (Pitch.Diatonic v) =
    PitchSignal.add_control Score.c_diatonic v

-- | Convert a Pitch to a NoteNumber, throwing an exception if the pitch
-- failed.
pitch_nn :: PitchSignal.Pitch -> Derive.Deriver Pitch.NoteNumber
pitch_nn = either (Derive.throw . ("evaluating pitch: " ++) . Pretty.pretty)
    return . PitchSignal.pitch_nn
