-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of basic low level pitch calls.
--
-- Low level calls should do simple orthogonal things and their names are
-- generally just one or two characters.
module Derive.C.Prelude.Pitch (
    library
    , approach
) where
import qualified Data.Maybe as Maybe

import qualified Util.Doc as Doc
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Post as Post
import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Sig as Sig
import           Derive.Sig (defaulted, required)
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Ui.Events as Events
import qualified Ui.Types as Types

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators $
        [ ("set", cSet)
        , ("set-or-move", cSetOrMove)
        , ("'", cSetPrev)
        , ("*", cMultiply)

        -- interpolating
        , ("n", cNeighbor)
        , ("a", cApproach)
        , ("u", cUp)
        , ("d", cDown)
        , ("p", cPorta)
        ] ++ PitchUtil.interpolatorVariations
    , Library.transformers [("set", cSetTransformer)]
    ]

-- * pitch

cSet :: Derive.Generator Derive.Pitch
cSet = generator1 "set" mempty "Emit a pitch with no interpolation." $
    -- This could take a transpose too, but then set has to be in
    -- 'require_previous', it gets shadowed for "" because of scales that use
    -- numbers, and it's not clearly useful.
    Sig.call (required "pitch" "Set this pitch.") $ \pitch_ args -> do
        let pitch = either PSignal.nnPitch id pitch_
        pos <- Args.realStart args
        pure $ PSignal.fromSample pos pitch

cSetOrMove :: Derive.Generator Derive.Pitch
cSetOrMove = generator1 "set-or-move" mempty
    "Emit a pitch with no interpolation if it coincides with a note attack,\
    \ and interpolate otherwise." $
    Sig.call ((,)
        <$> required "pitch" "Set this pitch."
        <*> defaulted "time" (Typecheck.real 0.15) "Time to move to the pitch."
    ) $ \(pitch_, Typecheck.DefaultReal time) args -> do
        let pitch = either PSignal.nnPitch id pitch_
        (eventAt (Args.start args) <$> Args.getNoteEvents) >>= \case
            False -> do
                (start, end) <- Call.durationFromStart args time
                let maybeFrom = snd <$> Args.prevPitch args
                PitchUtil.makeSegmentFrom ControlUtil.Linear start maybeFrom
                    end (Left pitch)
            True -> do
                pos <- Args.realStart args
                pure $ PSignal.fromSample pos pitch

eventAt :: TrackTime -> Events.Events -> Bool
eventAt pos events = Maybe.isJust (Events.at pos Types.Positive events)
    || Maybe.isJust (Events.at pos Types.Negative events)

cSetTransformer :: Derive.Transformer Derive.Pitch
cSetTransformer = Derive.transformer Module.prelude "set" mempty
    "Prepend a pitch to a signal. This is useful to create a discontinuity,\
    \ e.g. interpolate to a pitch and then jump to another one."
    $ Sig.callt (required "pitch" "Set this pitch.")
    $ \pitch_ args deriver -> do
        let pitch = either PSignal.nnPitch id pitch_
        pos <- Args.realStart args
        Post.signal (<> PSignal.fromSample pos pitch) deriver

-- | Re-set the previous val.  This can be used to extend a breakpoint.
cSetPrev :: Derive.Generator Derive.Pitch
cSetPrev = Derive.generator Module.prelude "set-prev" Tags.prev
    "Re-set the previous pitch.  This can be used to extend a breakpoint."
    $ Sig.call0 $ \args -> do
        start <- Args.realStart args
        pure $ case Args.prevPitch args of
            Just (x, y) | start > x ->
                Stream.fromEvent $ PSignal.fromSample start y
            _ -> Stream.empty

cMultiply :: Derive.Generator Derive.Pitch
cMultiply = generator1 "multiply" mempty
    "Emit the given pitch multiplied by a factor."
    $ Sig.call ((,)
    <$> required "pitch" "Source pitch."
    <*> defaulted "interval" (Left 0 :: Either Pitch.Hz Pitch.Hz)
        (ScaleDegree.intervalArgDoc intervals)
    ) $ \(pitch, interval) args -> do
        interval <- ScaleDegree.resolveIntervals intervals [interval]
        scale <- Call.getScale
        let transposed = Pitches.modifyHz (Pitches.scale scale) (*interval)
                pitch
        start <- Args.realStart args
        pure $ PSignal.fromSample start transposed
    where
    intervals = JustScales.defaultNamedIntervals

-- * interpolating

cNeighbor :: Derive.Generator Derive.Pitch
cNeighbor = generator1 "neighbor" mempty
    "Emit a slide from a neighboring pitch to the given one."
    $ Sig.call ((,,,)
    <$> required "pitch" "Destination pitch."
    <*> defaulted "neighbor" (Pitch.Chromatic 1) "Neighobr interval."
    <*> defaulted "time" (Typecheck.real 0.1) "Time to get to destination."
    <*> ControlUtil.curveEnv
    ) $ \(pitch, neighbor, Typecheck.DefaultReal time, curve) args -> do
        (start, end) <- Call.durationFromStart args time
        let pitch1 = Pitches.transpose neighbor pitch
        PitchUtil.makeSegment curve start pitch1 end pitch

cApproach :: Derive.Generator Derive.Pitch
cApproach = generator1 "approach" Tags.next
    "Slide to the next pitch." $ Sig.call ((,)
    <$> defaulted "time" (Typecheck.real 0.2) "Time to get to destination."
    <*> ControlUtil.curveEnv
    ) $ \(Typecheck.DefaultReal time, curve) args -> do
        (start, end) <- Call.durationFromStart args time
        approach args curve start end

approach :: Derive.PitchArgs -> ControlUtil.Curve -> RealTime -> RealTime
    -> Derive.Deriver PSignal.PSignal
approach args curve start end = do
    maybeNext <- Args.evalNextPitch args
    case (Args.prevPitch args, maybeNext) of
        (Just (_, prev), Just next) ->
            PitchUtil.makeSegment curve start prev end next
        _ -> pure mempty

cUp :: Derive.Generator Derive.Pitch
cUp = generator1 "up" Tags.prev
    "Ascend at the given speed until the next event." $ slope "Ascend" 1

cDown :: Derive.Generator Derive.Pitch
cDown = generator1 "down" Tags.prev
    "Descend at the given speed until the next event." $ slope "Descend" (-1)

slope :: Doc.Doc -> Double -> Derive.WithArgDoc
    (Derive.PitchArgs -> Derive.Deriver PSignal.PSignal)
slope word sign = Sig.call ((,,)
    <$> defaulted "slope" (Pitch.Chromatic 1)
        (word <> " this many steps per second.")
    <*> PitchUtil.fromEnv <*> ControlUtil.curveEnv
    ) $ \(slope, from, curve) args -> case PitchUtil.prevVal from args of
        Nothing -> pure mempty
        Just prevPitch -> do
            start <- Args.realStart args
            next <- Derive.real (Args.next args)
            let dest = Pitches.transpose transpose prevPitch
                transpose = Pitch.modifyTranspose
                    (* (RealTime.toSeconds (next - start) * sign)) slope
            PitchUtil.makeSegment curve start prevPitch next dest

cPorta :: Derive.Generator Derive.Pitch
cPorta = generator1 "porta" mempty
    "Interpolate between two pitches. This is similar to `i>>`,  but intended\
    \ to be higher level, in that instruments or scores can override it to\
    \ represent an idiomatic portamento."
    $ Sig.call ((,,,,)
    <$> PitchUtil.pitchArg
    <*> defaulted "time" ControlUtil.defaultInterpolationTime
        "Time to reach destination."
    <*> Sig.defaultedEnv "place" Sig.Both (Typecheck.Normalized 1)
        "Placement, from before to after the call."
    <*> PitchUtil.fromEnv <*> ControlUtil.curveEnv
    ) $ \(to, Typecheck.DefaultReal time, place, from, curve) args -> do
        let maybeFrom = from <|> (snd <$> Args.prevPitch args)
        time <- if Args.duration args == 0
            then pure time
            else DeriveT.RealDuration <$> Args.realDuration args
        (start, end) <- ControlUtil.placeRange place (Args.start args) time
        PitchUtil.makeSegmentFrom curve start maybeFrom end to

-- * util

generator1 :: Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Derive.WithArgDoc (Derive.PassedArgs d -> Derive.Deriver d)
    -> Derive.Call (Derive.GeneratorFunc d)
generator1 = Derive.generator1 Module.prelude
