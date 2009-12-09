module Derive.DeriveTest where

import Ui
import qualified Derive.Scale.Twelve as Twelve
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal


scale_id = Twelve.scale_id

pitch_signal :: Pitch.ScaleId -> [PitchSignal.Segment]
    -> PitchSignal.PitchSignal
pitch_signal scale_id =
    PitchSignal.track_signal scale_id PitchSignal.default_srate

signal = Signal.track_signal Signal.default_srate
