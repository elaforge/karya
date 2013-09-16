-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Transformers on control and pitch signals.
module Derive.Call.SignalTransform where
import Util.Control
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Speed as Speed
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Sig as Sig
import Derive.Sig (required)

import qualified Perform.Signal as Signal
import Types


-- * pitch

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.call_maps [] [("sh", c_sh_pitch)]

c_sh_pitch :: Derive.Transformer Derive.Pitch
c_sh_pitch = Derive.transformer "sh" mempty
    "Sample & hold. Hold values at the given speed."
    $ Sig.callt Speed.arg $ \speed _args deriver -> do
        (sig, (start, end), logs) <- Post.pitch_range deriver
        starts <- Speed.starts speed (start, end)
        return $ LEvent.Event (sample_hold_pitch starts sig)
            : map LEvent.Log logs

sample_hold_pitch :: [RealTime] -> PitchSignal.Signal -> PitchSignal.Signal
sample_hold_pitch points sig = PitchSignal.unfoldr go (Nothing, points, sig)
    where
    go (_, [], _) = Nothing
    go (prev, x : xs, sig_) = case PitchSignal.head sig of
            Just (_, y) -> Just ((x, y), (Just y, xs, sig))
            Nothing -> case prev of
                Nothing -> go (prev, xs, sig)
                Just p -> Just ((x, p), (prev, xs, sig))
        where sig = PitchSignal.drop_before x sig_


-- * control

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.call_maps []
    [ ("quantize", c_quantize)
    , ("sh", c_sh_control)
    ]

c_sh_control :: Derive.Transformer Derive.Control
c_sh_control = Derive.transformer "sh" mempty
    "Sample & hold. Hold values at the given speed."
    $ Sig.callt Speed.arg $ \speed _args deriver -> do
        (sig, (start, end), logs) <- Post.control_range deriver
        starts <- Speed.starts speed (start, end)
        return $ LEvent.Event (sample_hold_control starts sig)
            : map LEvent.Log logs

sample_hold_control :: [RealTime] -> Signal.Control -> Signal.Control
sample_hold_control points sig = Signal.unfoldr go (0, points, sig)
    where
    go (_, [], _) = Nothing
    go (prev, x : xs, sig_) = case Signal.head sig of
            Just (_, y) -> Just ((x, y), (y, xs, sig))
            Nothing -> Just ((x, prev), (prev, xs, sig))
        where sig = Signal.drop_before x sig_

c_quantize :: Derive.Transformer Derive.Control
c_quantize = Derive.transformer "quantize" mempty
    "Quantize a control signal."
    $ Sig.callt (required "val" "Quantize to multiples of this value.") $
    \val _args -> Post.signal (quantize val)

quantize :: Signal.Y -> Signal.Control -> Signal.Control
quantize val
    | val == 0 = id
    | otherwise = Signal.map_y $ \y -> fromIntegral (round (y / val)) * val
