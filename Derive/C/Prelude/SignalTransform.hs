-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Transformers on control and pitch signals.
module Derive.C.Prelude.SignalTransform (
    library
    , slew_limiter
) where
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Speed as Speed
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


library :: Library.Library
library = mconcat
    [ Library.transformers [("cf-sample", c_cf_sample)]
    , Library.transformers [("sh", c_sh_pitch)]
    , Library.transformers
        [ ("quantize", c_quantize)
        , ("sh", c_sh_control)
        , ("slew", c_slew)
        , ("smooth", c_smooth)
        , ("->", c_redirect Derive.DefaultMerge)
        -- TODO should I set to 1 at start and end, like
        -- Control.multiply_signal?
        , ("->+", c_redirect (Derive.Merge Derive.merge_add))
        ]
    ]

-- * pitch

c_sh_pitch :: Derive.Transformer Derive.Pitch
c_sh_pitch = Derive.transformer Module.prelude "sh" mempty
    "Sample & hold. Hold values at the given speed."
    $ Sig.callt Speed.arg $ \speed _args deriver -> do
        (sig, (start, end), logs) <- Post.pitch_range deriver
        starts <- Speed.starts speed (start, end) True
        return $ Stream.from_event_logs (sample_hold_pitch starts sig) logs

-- TODO(polymorphic-signals): this is the same as 'sample_hold_control'
sample_hold_pitch :: [RealTime] -> PSignal.PSignal -> PSignal.PSignal
sample_hold_pitch points sig = PSignal.from_pairs $ do
    (x1, n) <- Seq.zip_next points
    Just y <- return $ PSignal.at x1 sig
    x <- x1 : maybe [] (:[]) n
    return (x, y)


-- * control

c_sh_control :: Derive.Transformer Derive.Control
c_sh_control = Derive.transformer Module.prelude "sh" mempty
    "Sample & hold. Hold values at the given speed."
    $ Sig.callt Speed.arg $ \speed _args deriver -> do
        (sig, (start, end), logs) <- Post.control_range deriver
        starts <- Speed.starts speed (start, end) True
        return $ Stream.from_event_logs (sample_hold_control starts sig) logs

sample_hold_control :: [RealTime] -> Signal.Control -> Signal.Control
sample_hold_control points sig = Signal.from_pairs $ do
    (x1, n) <- Seq.zip_next points
    let y = Signal.at x1 sig
    x <- x1 : maybe [] (:[]) n
    return (x, y)

c_quantize :: Derive.Transformer Derive.Control
c_quantize = Derive.transformer Module.prelude "quantize" mempty
    "Quantize a control signal."
    $ Sig.callt (Sig.required "val" "Quantize to multiples of this value.") $
    \val _args deriver -> do
        srate <- Call.get_srate
        Post.signal (quantize srate val) deriver

quantize :: RealTime -> Signal.Y -> Signal.Control -> Signal.Control
quantize srate val
    | val == 0 = id
    | otherwise = Signal.map_y srate
        (\y -> fromIntegral (round (y / val)) * val)

c_slew :: Derive.Transformer Derive.Control
c_slew = Derive.transformer Module.prelude "slew" mempty
    "Smooth a signal by interpolating such that it doesn't exceed the given\
    \ slope."
    $ Sig.callt (Sig.required "slope" "Maximum allowed slope, per second.")
    $ \slope _args -> Post.signal (slew_limiter slope)

-- | Smooth the signal by not allowing the signal to change faster than the
-- given slope.
slew_limiter :: Signal.Y -> Signal.Control -> Signal.Control
slew_limiter max_slope =
    Signal.from_pairs . snd . List.mapAccumL limit Nothing . Signal.to_pairs
    where
    limit Nothing (x, y) = (Just (x, y), (x, y))
    limit (Just (x0, y0)) (x1, y1)
        | abs slope <= max_slope = (Just (x1, y1), (x1, y1))
        | otherwise = (Just (x1, y), (x1, y))
        where
        y = dx * max_slope
        slope = (y1 - y0) / dx
        dx = RealTime.to_seconds (x1 - x0)

c_smooth :: Derive.Transformer Derive.Control
c_smooth = Derive.transformer Module.prelude "smooth" mempty
    "Smooth a signal by interpolating between each sample."
    $ Sig.callt ((,)
    <$> Sig.required "time" "Amount of time to reach to the next sample.\
        \ If negative, it will end on the destination sample rather than\
        \ start on it. The time will be compressed if the samples are too\
        \ close, so unlike `slew`, this will always reach the samples in the\
        \ source."
    <*> ControlUtil.curve_arg
    ) $ \(Typecheck.DefaultReal time, curve) args deriver -> do
        srate <- Call.get_srate
        time <- Call.real_duration (Args.start args) time
        Post.signal (ControlUtil.smooth_absolute curve srate time
            . Signal.to_pairs_unique) deriver

c_redirect :: Derive.Merge -> Derive.Transformer Derive.Control
c_redirect merger =
    Derive.transformer Module.prelude "redirect" Tags.cmod
    ("Redirect a signal to another control, using the control modifier hack.\
    \ The control is combined with " <> merge_name merger <> ".")
    $ Sig.callt (Sig.required "control" "Redirect to this control.")
    $ \control _args deriver -> do
        (sig, logs) <- Post.derive_signal deriver
        merger <- Derive.resolve_merge merger control
        Derive.modify_control merger control sig
        return $ Stream.from_logs logs
    where
    merge_name Derive.DefaultMerge = "the default merger for the control"
    merge_name (Derive.Merge merger) = ShowVal.doc merger

c_cf_sample :: Derive.Transformer Derive.Note
c_cf_sample = Derive.transformer Module.prelude "cf-sample"
    Tags.control_function
    "Sample the given control functions and insert them as constants in the\
    \ control map. The default note call expects continuous signals, so it\
    \ takes slices out of the control map. This transfers control functions\
    \ to the control map, so you can e.g. use randomized controls."
    $ Sig.callt (Sig.many1 "control" "Sample these control functions.")
    $ \controls args deriver -> do
        start <- Args.real_start args
        vals <- mapM (flip Derive.control_at start) (NonEmpty.toList controls)
        foldr (uncurry Call.with_constant) deriver
            [ (c, Score.typed_val v)
            | (c, Just v) <- zip (NonEmpty.toList controls) vals
            ]
