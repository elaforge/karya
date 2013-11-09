-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Carnatic style pitch ornaments.
module Derive.Call.Gamakam where
import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Call.SignalTransform as SignalTransform
import qualified Derive.Call.Trill as Trill
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, required)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Types

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.call_maps
    [ ("wobble", c_wobble)
    ]
    []

-- TODO note version that ends on up or down
c_wobble :: Derive.Generator Derive.Pitch
c_wobble = Derive.generator1 "wobble" mempty
    "This is a kind of trill, but its interval is in NNs regardless of the\
    \ scale, and transitions between the notes are smooth.  It's intended for\
    \ the vocal microtonal trills common in Carnatic music."
    $ Sig.call ((,,,)
    <$> required "note" "Base pitch."
    <*> defaulted "neighbor" (Sig.control "trill-neighbor" 1)
        "Alternate with a pitch at this interval."
    <*> defaulted "speed" (Sig.typed_control "trill-speed" 6 Score.Real)
        "Alternate pitches at this speed."
    <*> defaulted "slope" 8
        "Pitch moves at a maximum of this many NNs per second."
    ) $ \(note, neighbor, speed, slope) args -> do
        srate <- Util.get_srate
        transpose <- SignalTransform.slew_limiter srate slope <$>
            trill_signal (Args.range_or_next args) neighbor speed
        start <- Args.real_start args
        return $ PitchSignal.apply_control Controls.nn
            (Score.untyped transpose) $ PitchSignal.signal [(start, note)]

trill_signal :: (ScoreTime, ScoreTime) -> TrackLang.ValControl
    -> TrackLang.ValControl -> Derive.Deriver Signal.Control
trill_signal range neighbor speed = do
    transitions <- Trill.trill_transitions range speed
    neighbor_sig <- Util.to_untyped_signal neighbor
    return $ trill_from_transitions transitions neighbor_sig

-- | Make a trill signal from a list of transition times.
trill_from_transitions :: [RealTime] -> Signal.Control -> Signal.Control
trill_from_transitions transitions neighbor =
    Signal.signal [(x, if t then Signal.at x neighbor else 0)
        | (x, t) <- zip transitions (cycle [False, True])]
