-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Transformers on control and pitch signals.
module Derive.C.Prelude.SignalTransform (
    library
    , slewLimiter
) where
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Lists as Lists
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
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Types


library :: Library.Library
library = mconcat
    [ Library.transformers [("cf-sample", cCfSample)]
    , Library.transformers [("sh", cShPitch)]
    , Library.transformers
        [ ("quantize", cQuantize)
        , ("sh", cShControl)
        , ("slew", cSlew)
        , ("smooth", cSmooth)
        , ("->", cRedirect Derive.DefaultMerge)
        -- TODO should I set to 1 at start and end, like
        -- Control.multiply_signal?
        , ("->+", cRedirect (Derive.Merge Derive.mergeAdd))
        ]
    ]

-- * pitch

cShPitch :: Derive.Transformer Derive.Pitch
cShPitch = Derive.transformer Module.prelude "sh" mempty
    "Sample & hold. Hold values at the given speed."
    $ Sig.callt Speed.arg $ \speed _args deriver -> do
        (sig, (start, end), logs) <- Post.pitchRange deriver
        starts <- Speed.starts speed (start, end) True
        pure $ Stream.fromEventLogs (sampleHoldPitch starts sig) logs

-- TODO(polymorphic-signals): this is the same as 'sample_hold_control'
sampleHoldPitch :: [RealTime] -> PSignal.PSignal -> PSignal.PSignal
sampleHoldPitch points sig = PSignal.fromPairs $ do
    (x1, n) <- Lists.zipNext points
    Just y <- pure $ PSignal.at sig x1
    x <- x1 : maybe [] (:[]) n
    pure (x, y)


-- * control

cShControl :: Derive.Transformer Derive.Control
cShControl = Derive.transformer Module.prelude "sh" mempty
    "Sample & hold. Hold values at the given speed."
    $ Sig.callt Speed.arg $ \speed _args deriver -> do
        (sig, (start, end), logs) <- Post.controlRange deriver
        starts <- Speed.starts speed (start, end) True
        pure $ Stream.fromEventLogs (sampleHoldControl starts sig) logs

sampleHoldControl :: [RealTime] -> Signal.Control -> Signal.Control
sampleHoldControl points sig = Signal.fromPairs $ do
    (x1, n) <- Lists.zipNext points
    let y = Signal.at sig x1
    x <- x1 : maybe [] (:[]) n
    pure (x, y)

cQuantize :: Derive.Transformer Derive.Control
cQuantize = Derive.transformer Module.prelude "quantize" mempty
    "Quantize a control signal."
    $ Sig.callt (Sig.required "val" "Quantize to multiples of this value.") $
    \val _args deriver -> do
        srate <- Call.getSrate
        Post.signal (quantize srate val) deriver

-- This would need to change for linear segments: for each segment, find where
-- it crosses the threshold between multiples, draw flat segments at each one.
-- But it may be that I want to do a soft quantize, where I map through
-- a function which is attracted to the quantize points, and that will need
-- resampling anyway.  So I'll leave this as-is.
quantize :: RealTime -> Signal.Y -> Signal.Control -> Signal.Control
quantize srate val
    | val == 0 = id
    | otherwise = Signal.mapY srate
        (\y -> fromIntegral (round (y / val)) * val)

cSlew :: Derive.Transformer Derive.Control
cSlew = Derive.transformer Module.prelude "slew" mempty
    "Smooth a signal by interpolating such that it doesn't exceed the given\
    \ slope."
    $ Sig.callt (Sig.required "slope" "Maximum allowed slope, per second.")
    $ \slope _args -> Post.signal (slewLimiter slope)

-- | Smooth the signal by not allowing the signal to change faster than the
-- given slope.
slewLimiter :: Signal.Y -> Signal.Control -> Signal.Control
slewLimiter maxSlope =
    Signal.fromPairs . snd . List.mapAccumL limit Nothing . Signal.toPairs
    where
    limit Nothing (x, y) = (Just (x, y), (x, y))
    limit (Just (x0, y0)) (x1, y1)
        | abs slope <= maxSlope = (Just (x1, y1), (x1, y1))
        | otherwise = (Just (x1, y), (x1, y))
        where
        y = dx * maxSlope
        slope = (y1 - y0) / dx
        dx = RealTime.toSeconds (x1 - x0)

-- TODO maybe a bit broken since signals are now usually continuous?
-- Fix it when necessary.
cSmooth :: Derive.Transformer Derive.Control
cSmooth = Derive.transformer Module.prelude "smooth" mempty
    "Smooth a signal by interpolating between discontinuities."
    $ Sig.callt ((,)
    <$> Sig.required "time" "Amount of time to reach to the next sample.\
        \ If negative, it will end on the destination sample rather than\
        \ start on it. The time will be compressed if the samples are too\
        \ close, so unlike `slew`, this will always reach the samples in the\
        \ source."
    <*> ControlUtil.curveArg
    ) $ \(Typecheck.DefaultReal time, curve) args deriver -> do
        srate <- Call.getSrate
        time <- Call.realDuration (Args.start args) time
        Post.signal (ControlUtil.smoothAbsolute curve srate time
            . Lists.dropInitialDups fst . Signal.toPairs) deriver

cRedirect :: Derive.Merge -> Derive.Transformer Derive.Control
cRedirect merger =
    Derive.transformer Module.prelude "redirect" Tags.cmod
    ("Redirect a signal to another control, using the control modifier hack.\
    \ The control is combined with " <> mergeName merger <> ".")
    $ Sig.callt (Sig.required "control" "Redirect to this control.")
    $ \control _args deriver -> do
        (sig, logs) <- Post.deriveSignal deriver
        merger <- Derive.resolveMerge merger control
        Derive.modifyControl merger control sig
        pure $ Stream.fromLogs logs
    where
    mergeName Derive.DefaultMerge = "the default merger for the control"
    mergeName (Derive.Merge merger) = ShowVal.doc merger

cCfSample :: Derive.Transformer Derive.Note
cCfSample = Derive.transformer Module.prelude "cf-sample"
    Tags.controlFunction
    "Sample the given control functions and insert them as constants in the\
    \ control map. The default note call expects continuous signals, so it\
    \ takes slices out of the control map. This transfers control functions\
    \ to the control map, so you can e.g. use randomized controls."
    $ Sig.callt (Sig.many1 "control" "Sample these control functions.")
    $ \controls args deriver -> do
        start <- Args.realStart args
        vals <- mapM (flip Derive.controlAt start) (NonEmpty.toList controls)
        foldr (uncurry Call.withConstant) deriver
            [ (c, ScoreT.valOf v)
            | (c, Just v) <- zip (NonEmpty.toList controls) vals
            ]
